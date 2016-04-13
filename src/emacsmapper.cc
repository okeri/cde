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

emacsMapper& emacsMapper::inst() {
    static emacsMapper instance;
    return instance;
}


std::vector<emacsMapper::LLVMRemappedFile> &emacsMapper::mapped() {
    return inst().mapped_;
}


void emacsMapper::map(const std::string &filename, size_t size) {
    unmap(filename);

    llvm::MemoryBuffer *buffer =
            llvm::MemoryBuffer::getNewUninitMemBuffer(size).release();
    if (size) {
        std::cin.read(const_cast<char*>(buffer->getBufferStart()), size);
    }
    inst().mapped_.emplace_back(filename, buffer);
}


void emacsMapper::unmap(const std::string &filename) {
    auto &mapped = inst().mapped_;
    const auto &removed  = remove_if(mapped.begin(), mapped.end(),
                           [filename](const auto& f) {
                               return f.first == filename;
                               });
    if (removed != mapped.end()) {
        mapped.erase(removed);
    }
}
