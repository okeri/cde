/*
  CDE - C/C++ development environment for emacs
  Copyright (C) 2016-2018 Oleg Keri

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

#include <cstdint>
#include <string>
#include <string_view>
#include <forward_list>

enum {INVALID = 0xffffffff};

namespace fileutil {

enum {MAX_DISP_LEN = 80};

bool hasTail(std::string_view str, std::string_view end);
std::string join(std::string_view first, std::string_view second);
std::string joinp(std::string_view first, std::string_view second);
std::string dirUp(std::string_view path);
std::string purify(std::string_view path);
std::string basenameNoExt(std::string_view filename);
std::string extension(std::string_view filename);
uint32_t fileTime(std::string_view filename);
bool fileExists(std::string_view filename);
void collectFiles(std::string_view path,
                  std::forward_list<std::string> *files, bool checkExt = true);
const char *findLineInFile(const std::string &filename, uint32_t ofs);
const char* extractPosInFile(
    const std::string& filename, uint32_t start, uint32_t end, uint32_t skip);
const char* extractPosInString(
    std::string_view data, uint32_t start, uint32_t end, uint32_t skip);
void mkdir(std::string_view path);
}  // namespace fileutil
