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

#include <map>
#include "single.h"

class EmacsMapper : public Single {
    using RemappedFiles = std::map<std::string,
                                   std::pair<std::string, uint32_t>>;

    RemappedFiles mapped_;

    static EmacsMapper& inst();

  public:
    static RemappedFiles &mapped();
    static void map(const std::string &, size_t);
    static void unmap(const std::string &);
};
