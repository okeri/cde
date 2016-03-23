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
#include <string>

template <typename Pred>
void strBreak(const std::string inp, Pred handler, const size_t start = 0,
              const size_t endpoint = std::string::npos) {
    size_t tail = start, end = endpoint == std::string::npos ?
            inp.length() : endpoint, head = end;
    for (; tail <= end; ++tail) {
        if (isgraph(inp[tail])) {
            if (head == end) {
                head = tail;
            }
        } else {
            if (head != end) {
                size_t len = tail - head;
                if (!handler(inp.c_str() + head, len)) {
                    return;
                }
                head = end;
            }
        }
    }
}
