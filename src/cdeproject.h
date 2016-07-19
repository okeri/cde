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

#define PRJ_EASY             ".clang_complete"
#define PRJ_CCJ        "compile_commands.json"

class CDEProject {
    Db db_;
    CDEIndex *index_;

  public:
    CDEProject(const std::string &projectPath,
               const std::string &store, bool pch);
    ~CDEProject();
    bool fileInProject(const std::string &filename) const;
    void scanProject();
    void check(const std::string &filename);
    void updateProjectFile(const std::string &filename);
    void definition(const std::string &filename, uint32_t pos);
    void references(const std::string &filename, uint32_t pos);
    void findfile(const std::string &filename, const std::string &parent);
    void swapSrcHdr(const std::string &filename);
    void acknowledge(const std::string &filename);
    void completion(const std::string &filename, const std::string &prefix,
                    uint32_t line, uint32_t column);

    static std::string findProjectRoot(const std::string &projectPath);
};
