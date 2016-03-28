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

CDEIndex *createIndex(const string& projectPath, const string& storePath,
                     bool pch);

CDEProject::CDEProject(const string &projectPath, const string &store,
                             bool pch)
        : db_(NULL, 0) {

    // init database
    string dbpath(store + SEPARATOR);
    size_t offset = dbpath.length();

    dbpath += projectPath;

    for (auto it = begin(dbpath) + offset; it != end(dbpath); ++it) {
        if (*it == SEPARATOR) {
            *it = '!';
        }
    }

    index_ = createIndex(projectPath, dbpath, pch);
    dbpath += ".cache";
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
            pack =
                    static_cast<SourceInfo::SourceInfoPacked*>(data.get_data());
            index_->files_.emplace(
                piecewise_construct, make_tuple(pack->filename),
                make_tuple(*static_cast<uint32_t*>(key.get_data()),
                           &index_->fileInfo(pack->pid)->second,
                           pack->updated_time));
        }
        res = curs->get(&key, &data, DB_NEXT);
    }
    curs->close();

    // load proj values
    ifstream f(projectPath + SEPARATOR + PRJ_EASY);
    if (f) {
        string content((istreambuf_iterator<char>(f)),
                       istreambuf_iterator<char>());
        index_->setGlobalArgs(content);
    }
    if (pch) {
        index_->loadPCHData();
    }
    // TODO: implement compile_commands.json handling
}


string CDEProject::findProjectRoot(const string &projectPath) {
    for (string root = projectPath; root != "";
         root = fileutil::dirUp(root)) {
        if (fileutil::fileExists(root + PRJ_EASY) ||
            fileutil::fileExists(root + PRJ_CCJ)) {
            return root;
        }
    }
    return projectPath;
}


void CDEProject::updateProjectFile(const SourceIter &si, size_t unsavedSize,
                                   bool noTimeCheck, uint32_t line) {
    string unsaved;
    readFromStdIn(unsavedSize, &unsaved);
    index_->parse(si, unsaved, false, noTimeCheck, line);
}


void CDEProject::updateProjectFile(const string &filename, size_t unsavedSize,
                                   bool noTimeCheck, uint32_t line) {
    updateProjectFile(index_->getTUFile(filename.c_str()), unsavedSize,
                      noTimeCheck, line);
}


void CDEProject::definition(const string &filename, uint32_t pos,
                            size_t unsavedSize, bool forceReparse) {
    const SourceIter &si = index_->getTUFile(filename);
    updateProjectFile(si, unsavedSize, forceReparse);
    CI_KEY ref({si->second.getId(), pos});
    const auto& defIt = index_->records_.find(ref);
    if (defIt != index_->records_.end()) {
        const CI_DATA &def = defIt->second;
        cout << "(prog1 (find-file \"";
        cout << index_->fileName(def.file)
             << "\")(goto-char (point-min))(forward-char " << def.pos
             << ")(push (list \"" << filename
             << "\" " << pos << ") cde--ring))" << endl;
    } else {
        cout << "(dframe-message \"No definition found\")" << endl;
    }
}


void CDEProject::references(const string &filename, uint32_t pos,
                            size_t unsavedSize, bool forceReparse) {
    const SourceIter &si = index_->getTUFile(filename);
    updateProjectFile(si, unsavedSize, forceReparse);
    uint32_t file = si->second.getId(),
            dfile = INVALID, dpos;
    map<CI_KEY, uint32_t> results;

    for (const auto& r: index_->records_) {
        if (r.second.pos == pos && r.second.file == file) {
            results[r.first] = r.second.refline;
            if (r.second.flags & DF_FWD) {
                dfile = r.first.file;
                dpos = r.first.pos;
            }
        }
    }

    if (dfile != INVALID) {
        for (const auto& r: index_->records_) {
            if (r.second.pos == dpos && r.second.file == dfile &&
                (r.first.pos != pos || r.first.file != file)) {
                results[r.first] = r.second.refline;
            }
        }
    }

    if (results.empty()) {
        cout << "(message \"No references found\")" << endl;
    } else {
        string last = "", current;
        cout << "(cde--ref-setup '(";
        for (const auto& r: results) {
            current = index_->fileName(r.first.file);
            if (last != current) {
                cout << "\"" << current << "\" ";
                last = current;
            }
            cout << "(" << r.second << " "
                 << quoted(fileutil::findLineInFile(current, r.first.pos)) << ") ";
        }
        cout << "))" << endl;
    }
}

// TODO: use weighs calculation for findfile/swapSrcHdr
void CDEProject::findfile(const string &filename, const string &parent) {
    const SourceIter &it = index_->findFile(filename);
    if (it != index_->files_.end()) {
        cout << "(find-file \"" << it->first << "\")" << endl;
    } else {
        const SourceIter &pit = index_->getTUFile(parent);
        unordered_set<string> includes;
        includes.insert(fileutil::dirUp(parent));
        pit->second.fillIncludes(&includes);
        for (const auto& include_path : includes) {
            string test = include_path;
            fileutil::addTrailingSep(&test);
            test += filename;
            if (fileutil::fileExists(test)) {
                index_->getFile(test, &pit->second);
                cout << "(find-file \"" << test << "\")" << endl;
                return;
            }
        }
        cout << "(message \"file " << filename << " not found\")" << endl;
    }
}

void CDEProject::swapSrcHdr(const string &filename) {
    static string exts[][9] = {
        {".c", ".h", ".H", ""},
        {".cc", ".hh", ".h", ".H", ""},
        {".cpp", ".hpp", ".h", ".hh", ".H", ""},
        {".cxx", ".hxx", ".h", ".hh", ".H", ""},
        {".c++",".h++", ".h", ""},
        {".C", ".H", ".h", ""},
        {".h", ".c", ".cc", ".cpp", ".cxx", ".c++", ".C", ""},
        {".hh", ".cc", ".cpp", ".cxx", ""},
        {".hpp", ".cpp", ""},
        {".hxx", ".cxx", ""},
        {".h++", ".c++", ""},
        {".H", ".C", ".c", ".cc", ".cpp", ".cxx", ""},
        {""}
    };

    const string &ext = fileutil::extension(filename);
    if (ext == "") {
        cout << "(message \"file " << filename << " have no extension\")"
             << endl;
        return;
    }

    const string &base  = fileutil::basenameNoExt(filename);

    for (unsigned extIter = 0; exts[extIter][0] != ""; ++extIter) {
        if (exts[extIter][0] == ext) {
            const SourceIter &it = index_->getTUFile(filename);
            unordered_set<string> includes;
            string test;
            includes.insert(fileutil::dirUp(filename));
            it->second.fillIncludes(&includes);
            for (const auto& include_path : includes) {
                test = include_path;
                fileutil::addTrailingSep(&test);
                test += base;
                for (unsigned i = 1; exts[extIter][i] != ""; ++i) {
                    string testfile = test + exts[extIter][i];
                    if (fileutil::fileExists(testfile)) {
                        index_->getTUFile(testfile);
                        cout << "(find-file \"" << testfile << "\")" << endl;
                        return;
                     }
                }
            }
            goto end;
        }
    }
end:
    cout << "(message \"cannot find pair for " << filename << "\")" << endl;
}

bool CDEProject::fileInProject(const string &filename) const {
    return index_->findFile(filename) != index_->files_.end();
}


void CDEProject::acknowledge(const string &filename) {
    cout << "(setq-local cde--project \"" << index_->projectPath() << "\")"
         << endl;
    updateProjectFile(filename, 0, true);
}

void CDEProject::scanProject() {
    forward_list<string> files;
    fileutil::collectFiles(index_->projectPath(), &files);
    for (const auto& it: files) {
        cout << "(dframe-message \"parsing " << it << "\")" << endl;
        updateProjectFile(it.c_str(), 0, false);
    }
    cout << "(dframe-message \"Done!\")" << endl;
}


void CDEProject::readFromStdIn(size_t size, string* buf) {
    if (size) {
        buf->resize(size);
        cin.read(const_cast<char*>(buf->data()), size);
    }
}


void CDEProject::check(const string &filename, uint32_t line,
                       size_t unsavedSize) {
    updateProjectFile(filename, unsavedSize, true, line);
}


void CDEProject::completion(const string &filename, const string &prefix,
                               uint32_t line, uint32_t column,
                               size_t unsavedSize) {
    string unsaved;
    readFromStdIn(unsavedSize, &unsaved);
    index_->completion(index_->getTUFile(filename), prefix, line, column,
                       unsaved);
}

CDEProject::~CDEProject() {
    // Store file info
    uint32_t num;
    Dbt key(&num, sizeof(uint32_t));
    unsigned char pack[512];

    for (const auto& it: index_->files_) {
        uint32_t size = it.second.fillPack(it.first, pack);
        Dbt data(pack, size);
        num = it.second.getId();
        db_.put(NULL, &key, &data, 0);
    }

    // Store ref-def pairs
    for (auto it: index_->records_) {
        Dbt key(const_cast<CI_KEY*>(&it.first), sizeof(CI_KEY));
        Dbt data(const_cast<CI_DATA*>(&it.second), sizeof(CI_DATA));
        db_.put(NULL, &key, &data, 0);
    }
    delete index_;

    cout << "(prog1 (setq cde--process nil)"
         << "(save-buffers-kill-terminal))" << endl;
}
