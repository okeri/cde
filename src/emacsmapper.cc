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
#include <algorithm>
#include "emacsmapper.h"

EmacsMapper& EmacsMapper::inst() {
    static EmacsMapper instance;
    return instance;
}

EmacsMapper::RemappedFiles &EmacsMapper::mapped() {
    return inst().mapped_;
}

void EmacsMapper::map(const std::string &filename, size_t size) {
    std::string buffer(size, 0);
    if (size) {
        std::cin.read(const_cast<char *>(buffer.c_str()), size);
    }
    inst().mapped_[filename] = std::make_pair(
        buffer, static_cast<uint32_t>(time(NULL)));
}

void EmacsMapper::unmap(const std::string &filename) {
    inst().mapped_.erase(filename);
}
