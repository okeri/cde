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
#include <memory>
#include <string_view>
#include "cdeindex.h"

class CDEProject {
    Db db_;
    bool nocache_;
    std::unique_ptr<CDEIndex> index_;

  public:
    CDEProject(std::string_view projectPath,
               std::string_view store, bool nocache, bool pch);
    ~CDEProject();
    bool fileInProject(std::string_view filename) const;
    void scanProject();
    void check(std::string_view filename);
    void definition(std::string_view filename, uint32_t pos);
    void references(std::string_view filename, uint32_t pos);
    void findfile(std::string_view filename, std::string_view parent);
    void swapSrcHdr(std::string_view filename);
    void acknowledge(std::string_view filename);
    void completion(std::string_view filename, std::string_view prefix,
                    uint32_t line, uint32_t column);

    static std::string findProjectRoot(std::string_view projectPath);
};
