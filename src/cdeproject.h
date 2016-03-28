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

#pragma once

#include <db_cxx.h>
#include "cdeindex.h"

#define PRJ_EASY                        ".prj"
#define PRJ_CCJ        "compile_commands.json"

class CDEProject {
    Db db_;
    CDEIndex *index_;
    void readFromStdIn(size_t size, string* buf);
    void updateProjectFile(const SourceIter &si, size_t unsavedSize, bool
                           noTimeCheck);

  public:
    CDEProject(const string &projectPath, const string &store, bool pch);
    ~CDEProject();
    bool fileInProject(const string &filename) const;
    void scanProject();
    void updateProjectFile(const string &filename, size_t unsavedSize,
                           bool noTimeCheck);
    void definition(const string &filename, uint32_t pos, size_t unsavedSize,
                    bool forceReparse);
    void references(const string &filename, uint32_t pos, size_t unsavedSize,
                    bool forceReparse);
    void findfile(const string &filename, const string &parent);
    void swapSrcHdr(const string &filename);
    void acknowledge(const string &filename);
    void completion(const string &filename, const string &prefix, uint32_t line,
                    uint32_t column, size_t unsavedSize);
  public:
    static string findProjectRoot(const string &projectPath);
};
