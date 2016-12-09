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

#include <cstdint>
#include <string>
#include <forward_list>

#ifdef _WIN32
#define SEPARATOR                      '\\'
#else
#define SEPARATOR                       '/'
#endif

#define INVALID                  0xffffffff

namespace fileutil {

enum {MAX_DISP_LEN = 80};

bool endsWith(const std::string &str, const std::string &end, const char prev = 0);
bool isHeader(const std::string &path);
void deleteTrailingSep(std::string *path);
void addTrailingSep(std::string *path);
std::string dirUp(const std::string &path);
std::string purify(const std::string &path);
std::string basenameNoExt(const std::string &filename);
std::string extension(const std::string &filename);
uint32_t fileTime(const std::string &filename);
bool fileExists(const std::string &filename);
void collectFiles(const std::string &path,
                  std::forward_list<std::string> *files, bool checkExt = true);
const char * findLineInFile(const std::string &filename, uint32_t ofs);
void mkdir(const std::string& path);
}  // namespace fileutil
