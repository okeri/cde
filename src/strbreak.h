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
              const size_t end = std::string::npos) {
    size_t tail = 0, len = end == std::string::npos ? inp.length() : end,
            head = len;
    for (; tail <= len; tail++) {
        if (isgraph(inp[tail])) {
            if (head == len) {
                head = tail;
            }
        } else {
            if (head != len) {
                size_t nlen = tail - head;
                if (!handler(inp.c_str() + head, nlen)) {
                    return;
                }
                head = len;
            }
        }
    }
}
