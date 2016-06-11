/*
  CDE - C/C++ development environment for emacs
  Copyright (C) 2016 Oleg Keri

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>
*/

#include <iostream>
#include <iomanip>
#include <fstream>
#include "fileutil.h"
#include "cdeproject.h"

CDEIndex *createIndex(const std::string& projectPath,
                      const std::string& storePath, bool pch);

#if DB_VERSION_MAJOR > 5
static int BDBKeyCmp(DB *db, const DBT *dbt1, const DBT *dbt2, size_t *locp) {
#else
static int BDBKeyCmp(DB *db, const DBT *dbt1, const DBT *dbt2) {
#endif
    if (dbt1->size == dbt2->size) {
         if (dbt1->size == sizeof(uint32_t)) {
            return *static_cast<int*>(dbt1->data) -
                    *static_cast<int*>(dbt2->data);
        } else {
            CI_KEY *k1(static_cast<CI_KEY*>(dbt1->data)),
                    *k2(static_cast<CI_KEY*>(dbt2->data));
            if (*k1 < *k2) {
                return -1;
            } else if (*k2 < *k1) {
                return 1;
            }
            return 0;
        }
    }
    return dbt1->size - dbt2->size;
}

CDEProject::CDEProject(const std::string &projectPath, const std::string &store,
                             bool pch)
        : db_(NULL, 0) {
    // init database
    std::string dbpath(store + SEPARATOR);
    size_t offset = dbpath.length();
    dbpath += projectPath;
    for (auto it = begin(dbpath) + offset; it != end(dbpath); ++it) {
        if (*it == SEPARATOR) {
            *it = '!';
        }
    }

    index_ = createIndex(projectPath, dbpath, pch);
    dbpath += ".cache";
    db_.set_bt_compare(BDBKeyCmp);
    db_.open(NULL, dbpath.c_str(), NULL, DB_BTREE, DB_CREATE, 0);

    // read index
    Dbc *curs;
    Dbt key, data;
    SourceInfo::SourceInfoPacked *pack;
    db_.cursor(NULL, &curs, 0);
    int res = curs->get(&key, &data, DB_FIRST);
    while (res != DB_NOTFOUND) {
        if (key.get_size() == sizeof(CI_KEY)) {
            index_->records_[*static_cast<CI_KEY*>(key.get_data())] =
                    *static_cast<CI_DATA*>(data.get_data());
        } else if (key.get_size() == sizeof(uint32_t)) {
            pack =  static_cast<SourceInfo::SourceInfoPacked*>(data.get_data());
            index_->push(*static_cast<uint32_t*>(key.get_data()),
                         pack->filename(), pack->updated_time,
                         pack->parent_count, pack->parents());
        }
        res = curs->get(&key, &data, DB_NEXT);
    }
    curs->close();

    // load proj values
    std::ifstream f(projectPath + SEPARATOR + PRJ_EASY);
    if (f) {
        std::string content((std::istreambuf_iterator<char>(f)),
                            std::istreambuf_iterator<char>());
        index_->setGlobalArgs(content);
    }
    if (pch) {
        index_->loadPCHData();
    }
    // TODO: implement compile_commands.json handling
}


std::string CDEProject::findProjectRoot(const std::string &projectPath) {
    for (std::string root = projectPath; root != "";
         root = fileutil::dirUp(root)) {
        if (fileutil::fileExists(root + PRJ_EASY) ||
            fileutil::fileExists(root + PRJ_CCJ)) {
            return root;
        }
    }
    return projectPath;
}


void CDEProject::updateProjectFile(const std::string &filename) {
    index_->parse(index_->getFile(filename), true);
}


void CDEProject::definition(const std::string &filename, uint32_t pos) {
    CI_KEY ref({index_->getFile(filename), pos});
    index_->parse(ref.file, true);

    const auto& defIt = index_->records_.find(ref);
    if (defIt != index_->records_.end()) {
        const CI_DATA &def = defIt->second;
        std::cout << "(find-file \"";
        std::cout << index_->fileName(def.file)
             << "\")(goto-char (point-min))(forward-char " << def.pos
             << ")(push (list \"" << filename
             << "\" " << pos << ") cde--ring)" << std::endl;
    } else {
        std::cout << "(dframe-message \"No definition found\")" << std::endl;
    }
}


void CDEProject::references(const std::string &filename, uint32_t pos) {
    uint32_t file = index_->getFile(filename),
            dfile = INVALID, dpos;
    index_->parse(file, true);

    std::map<CI_KEY, uint32_t> results;
    for (const auto& r : index_->records_) {
        if (r.second.pos == pos && r.second.file == file) {
            results[r.first] = r.second.refline;
            if (r.second.flags & CI_DATA::Forward) {
                dfile = r.first.file;
                dpos = r.first.pos;
            }
        }
    }

    if (dfile != INVALID) {
        for (const auto& r : index_->records_) {
            if (r.second.pos == dpos && r.second.file == dfile &&
                (r.first.pos != pos || r.first.file != file)) {
                results[r.first] = r.second.refline;
            }
        }
    }

    if (results.empty()) {
        std::cout << "(message \"No references found\")" << std::endl;
    } else {
        std::string last = "", current;
        std::cout << "(cde--ref-setup '(";
        for (const auto& r : results) {
            current = index_->fileName(r.first.file);
            if (last != current) {
                std::cout << "\"" << current << "\" ";
                last = current;
            }
            std::cout << "(" << r.second << " "
                      << std::quoted(fileutil::findLineInFile(current,
                                                              r.first.pos))
                      << ") ";
        }
        std::cout << "))" << std::endl;
    }
}

// TODO: use score calculation for findfile/swapSrcHdr
void CDEProject::findfile(const std::string &filename,
                          const std::string &parent) {
    uint32_t file = index_->findFile(filename);
    if (file != INVALID) {
        std::cout << "(find-file \"" << index_->fileName(file)
                  << "\")" << std::endl;
    } else {
        uint32_t pfile = index_->getFile(parent);
        std::unordered_set<std::string> includes;
        includes.insert(fileutil::dirUp(parent));
        index_->fillIncludes(pfile, &includes);
        for (const auto& include_path : includes) {
            std::string test = include_path;
            fileutil::addTrailingSep(&test);
            test += filename;
            if (fileutil::fileExists(test)) {
                std::cout << "(find-file \"" << test << "\")" << std::endl;
                return;
            }
        }
        std::cout << "(message \"'" << filename << "' file not found\")"
                  << std::endl;
    }
}

void CDEProject::swapSrcHdr(const std::string &filename) {
    static std::string exts[][9] = {
        {".c", ".h", ".H", ""},
        {".cc", ".hh", ".h", ".H", ""},
        {".cpp", ".hpp", ".h", ".hh", ".H", ""},
        {".cxx", ".hxx", ".h", ".hh", ".H", ""},
        {".c++", ".h++", ".h", ""},
        {".C", ".H", ".h", ""},
        {".h", ".c", ".cc", ".cpp", ".cxx", ".c++", ".C", ""},
        {".hh", ".cc", ".cpp", ".cxx", ""},
        {".hpp", ".cpp", ""},
        {".hxx", ".cxx", ""},
        {".h++", ".c++", ""},
        {".H", ".C", ".c", ".cc", ".cpp", ".cxx", ""},
        {""}
    };

    const std::string &ext = fileutil::extension(filename);
    if (ext == "") {
        std::cout << "(message \"file " << filename << " have no extension\")"
             << std::endl;
        return;
    }

    const std::string &base  = fileutil::basenameNoExt(filename);

    for (unsigned extIter = 0; exts[extIter][0] != ""; ++extIter) {
        if (exts[extIter][0] == ext) {
            uint32_t file = index_->getFile(filename);
            std::unordered_set<std::string> includes;
            std::string test;
            includes.insert(fileutil::dirUp(filename));
            index_->fillIncludes(file, &includes);
            for (const auto& include_path : includes) {
                test = include_path;
                fileutil::addTrailingSep(&test);
                test += base;
                for (unsigned i = 1; exts[extIter][i] != ""; ++i) {
                    std::string testfile = test + exts[extIter][i];
                    if (fileutil::fileExists(testfile)) {
                        std::cout << "(find-file \"" << testfile << "\")"
                                  << std::endl;
                        return;
                     }
                }
            }
            std::cout << "(message \"cannot find pair for " << filename
                      << "\")" << std::endl;
        }
    }
    std::cout << "(message \"cannot find pair for " << filename
              << "\")" << std::endl;
}

bool CDEProject::fileInProject(const std::string &filename) const {
    return index_->findFile(filename) != INVALID;
}

void CDEProject::acknowledge(const std::string &filename) {
    std::cout << "(setq-local cde--project \"" << index_->projectPath() << "\")"
         << std::endl;
    index_->preprocess(index_->getFile(filename));
}

void CDEProject::scanProject() {
    std::forward_list<std::string> files;
    fileutil::collectFiles(index_->projectPath(), &files);
    for (const auto& it : files) {
        std::cout << "(dframe-message \"parsing " << it << "\")" << std::endl;
        updateProjectFile(it);
    }
    std::cout << "(dframe-message \"Done!\")" << std::endl;
}

void CDEProject::check(const std::string &filename) {
    index_->parse(index_->getFile(filename), false);
}

void CDEProject::completion(const std::string &filename,
                            const std::string &prefix,
                            uint32_t line, uint32_t column) {
    index_->completion(index_->getFile(filename), prefix, line, column);
}

CDEProject::~CDEProject() {
    // Store file info
    uint32_t num;
    Dbt key(&num, sizeof(uint32_t));
    Dbt data;
    unsigned char pack[2048];
    data.set_data(pack);

    for (auto it = index_->begin() + 1; it != index_->end(); ++it) {
        data.set_size(it->fillPack(pack, sizeof(pack)));
        num = it->getId();
        db_.put(NULL, &key, &data, 0);
    }

    // Store ref-def pairs
    key.set_size(sizeof(CI_KEY));
    data.set_size(sizeof(CI_DATA));

    for (const auto &it : index_->records_) {
        key.set_data(const_cast<CI_KEY*>(&it.first));
        data.set_data(const_cast<CI_DATA*>(&it.second));
        db_.put(NULL, &key, &data, 0);
    }
    delete index_;

    std::cout << "(setq cde--process nil)(save-buffers-kill-terminal)"
              << std::endl;
}
