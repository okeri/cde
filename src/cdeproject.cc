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
#include <map>
#include <exception>
#include "rapidjson/document.h"
#include "fileutil.h"
#include "cdeproject.h"

namespace {

#if DB_VERSION_MAJOR > 5
int BDBKeyCmp(DB *, const DBT *dbt1, const DBT *dbt2, size_t *locp) {
#else
int BDBKeyCmp(DB *, const DBT *dbt1, const DBT *dbt2) {
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

static const char easy_file_id[] = ".clang_complete";
static const char ccj_file_id[] = "compile_commands.json";

}  // namespace


CDEProject::CDEProject(std::string_view projectPath, std::string_view store,
                       bool nocache, bool pch)
        : db_(NULL, 0), nocache_(nocache) {
    // init database
    std::string dbpath(std::string(store) + SEPARATOR);
    size_t offset = dbpath.length();
    dbpath += projectPath;

    auto manglePath = [] (auto b, auto e) {
        for (auto it = b; it != e; ++it) {
            if (*it == SEPARATOR) {
                *it = '!';
            }
        }
    };

    manglePath(begin(dbpath) + offset, end(dbpath));
    index_ = std::make_unique<CDEIndex>(projectPath, dbpath, pch);
    dbpath += ".cache";
    db_.set_bt_compare(BDBKeyCmp);
    db_.open(NULL, dbpath.c_str(), NULL, DB_BTREE, DB_CREATE, 0);

    // read index
    Dbc *curs;
    Dbt key, data;
    SourceInfo::SourceInfoPacked *pack;
    db_.cursor(NULL, &curs, 0);
    int res = curs->get(&key, &data, DB_FIRST);
    bool recovery = false;
    while (res != DB_NOTFOUND) {
        if (key.get_size() == sizeof(CI_KEY)) {
            index_->set(static_cast<CI_KEY*>(key.get_data()),
                        static_cast<CI_DATA*>(data.get_data()));
        } else if (key.get_size() == sizeof(uint32_t)) {
            pack =  static_cast<SourceInfo::SourceInfoPacked*>(data.get_data());
            index_->push(*static_cast<uint32_t*>(key.get_data()),
                         pack->filename(), pack->updated_time,
                         pack->parent_count, pack->parents());
        }
        try {
            res = curs->get(&key, &data, DB_NEXT);
        } catch (std::exception &e) {
            recovery = true;
            break;
        }
    }
    curs->close();

    if (recovery) {
        db_.close(0);
        remove(dbpath.c_str());
        db_.open(NULL, dbpath.c_str(), NULL, DB_BTREE, DB_CREATE, 0);
    }

    // load proj values
    if (std::ifstream f(std::string(projectPath) + SEPARATOR + easy_file_id); f) {
        std::string content((std::istreambuf_iterator<char>(f)),
                            std::istreambuf_iterator<char>());
        index_->setGlobalArgs(content);
    } else {
        f.open(std::string(projectPath) + SEPARATOR + ccj_file_id);
        if (f) {
            std::string content((std::istreambuf_iterator<char>(f)),
                                std::istreambuf_iterator<char>());
            rapidjson::Document root;
            if (root.Parse(content.c_str()); root.IsArray()) {
                for (auto it = root.Begin(); it != root.End(); ++it) {
                    if (it->IsObject()) {
                        auto directory = it->FindMember("directory");
                        if (directory == it->MemberEnd()) {
                            continue;
                        }

                        auto command = it->FindMember("command");
                        if (command == it->MemberEnd()) {
                            continue;
                        }

                        auto file = it->FindMember("file");
                        if (file == it->MemberEnd()) {
                            continue;
                        }

                        std::string filename = file->value.GetString();
                        if (filename.empty()) {
                            continue;
                        }

                        if (filename[0] != '/') {
                            std::string dirname(directory->value.GetString());
                            fileutil::addTrailingSep(&dirname);
                            filename = fileutil::purify(dirname + filename);
                        }

                        std::vector<std::string> args;
                        std::vector<std::string> ignores{"-c", "-o"};
                        bool ignoreNext = true;  // skip just first
                        strBreak(command->value.GetString(),
                                 [&args, &ignores, &ignoreNext]
                                 (auto begin, auto end) {
                                     if (!ignoreNext) {
                                         std::string arg(begin, end);
                                         if (find(ignores.begin(),
                                                  ignores.end(), arg) ==
                                             ignores.end()) {
                                             args.emplace_back(arg);
                                         } else {
                                             ignoreNext = true;
                                         }
                                     } else {
                                         ignoreNext = false;
                                     }
                                     return true;
                                 });

                        index_->setUnitWithArgs(filename, std::move(args));
                    }
                }
            }
        }
    }

    if (pch) {
        index_->loadPCHData();
    }
}


std::string CDEProject::findProjectRoot(std::string_view projectPath) {
    for (std::string root = std::string(projectPath); root != "";
         root = fileutil::dirUp(root)) {
        if (fileutil::fileExists(root + easy_file_id) ||
            fileutil::fileExists(root + ccj_file_id)) {
            return root;
        }
    }
    return std::string(projectPath);
}


void CDEProject::definition(std::string_view filename, uint32_t pos) {
    CI_KEY ref({index_->getFile(filename), pos});
    std::cout << "(message \"Searching...\")" << std::endl;
    index_->parse(ref.file, CDEIndex::ParseOptions::Normal);

    if (const auto& defIt = index_->records().find(ref);
        defIt != index_->records().end()) {
        const CI_DATA &def = defIt->second;
        std::cout << "(find-file \"";
        std::cout << index_->fileName(def.file)
             << "\")(goto-char (point-min))(forward-char " << def.pos
             << ")(push (list \"" << filename
             << "\" " << pos << ") cde--ring)(message \"\")" << std::endl;
    } else {
        std::cout << "(message \"No definition found\")" << std::endl;
    }
}


void CDEProject::references(std::string_view filename, uint32_t pos) {
    uint32_t file = index_->getFile(filename),
            dfile = INVALID, dpos;
    std::cout << "(message \"Searching...\")" << std::endl;
    index_->parse(file, CDEIndex::ParseOptions::Recursive);

    std::map<CI_KEY, uint32_t> results;
    for (const auto&[key, value] : index_->records()) {
        if (value.pos == pos && value.file == file) {
            results[key] = value.refline;
            if (value.flags & CI_DATA::Forward) {
                dfile = key.file;
                dpos = key.pos;
            }
        }
    }

    if (dfile != INVALID) {
        for (const auto& [key, value] : index_->records()) {
            if (value.pos == dpos && value.file == dfile &&
                (key.pos != pos || key.file != file)) {
                results[key] = value.refline;
            }
        }
    }

    if (results.empty()) {
        std::cout << "(message \"No references found\")" << std::endl;
    } else {
        std::string last = "", current;
        std::cout << "(cde--ref-setup '(";
        for (const auto&[key, value] : results) {
            current = index_->fileName(key.file);
            if (last != current) {
                std::cout << "\"" << current << "\" ";
                last = current;
            }
            std::cout << "(" << value << " "
                      << std::quoted(fileutil::findLineInFile(current,
                                                              key.pos))
                      << ") ";
        }
        std::cout << "))(message \"\")" << std::endl;
    }
}

void CDEProject::findfile(std::string_view filename,
                          std::string_view parent) {
    if (uint32_t file = index_->findFile(filename); file != INVALID) {
        std::cout << "(find-file \"" << index_->fileName(file)
                  << "\")" << std::endl;
    } else {
        uint32_t pfile = index_->getFile(parent);
        std::vector<std::string> includes =
                index_->includes(pfile, fileutil::dirUp(parent));

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

void CDEProject::swapSrcHdr(std::string_view filename) {
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

    std::string ext = fileutil::extension(filename);
    if (ext == "") {
        std::cout << "(message \"file " << filename << " have no extension\")"
             << std::endl;
        return;
    }

    std::string base = fileutil::basenameNoExt(filename);
    for (unsigned extIter = 0; exts[extIter][0] != ""; ++extIter) {
        if (exts[extIter][0] == ext) {
            std::string test;
            for (unsigned i = 1; exts[extIter][i] != ""; ++i) {
                test = base + exts[extIter][i];

                if (auto found = index_->findFile(test); found != INVALID) {
                    std::cout << "(find-file \"" << index_->fileName(found)
                              << "\")" << std::endl;
                    return;
                }
            }

            uint32_t file = index_->getFile(filename);
            std::vector<std::string> includes =
                    index_->includes(file, fileutil::dirUp(filename));

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
        }
    }
    std::cout << "(message \"cannot find pair for " << filename
              << "\")" << std::endl;
}

bool CDEProject::fileInProject(std::string_view filename) const {
    return index_->findFile(filename) != INVALID;
}

void CDEProject::acknowledge(std::string_view filename) {
    std::cout << "(cde--ack \"" << filename << "\" \""
              << index_->projectPath() << "\")"
         << std::endl;
    index_->preprocess(index_->getFile(filename));
}

void CDEProject::scanProject() {
    std::forward_list<std::string> files;
    fileutil::collectFiles(index_->projectPath(), &files);
    for (const auto& it : files) {
        std::cout << "(message \"parsing " << it << "\")" << std::endl;
        index_->parse(index_->getFile(it), CDEIndex::ParseOptions::Forget);
    }
    std::cout << "(message \"Done!\")" << std::endl;
}

void CDEProject::check(std::string_view filename) {
    index_->parse(index_->getFile(filename), CDEIndex::ParseOptions::Force);
}

void CDEProject::completion(std::string_view filename,
                            std::string_view prefix,
                            uint32_t line, uint32_t column) {
    index_->completion(index_->getFile(filename), prefix, line, column);
}

CDEProject::~CDEProject() {
    if (nocache_) {
        return;
    }
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

    for (const auto &[k, v] : index_->records()) {
        key.set_data(const_cast<CI_KEY*>(&k));
        data.set_data(const_cast<CI_DATA*>(&v));
        db_.put(NULL, &key, &data, 0);
    }

    std::cout << "(setq cde--process nil)" << std::endl;
}
