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

#include <string_view>

template <typename Pred>
void strBreak(std::string_view input, Pred handler, const size_t start = 0,
              const size_t endpoint = std::string_view::npos) {
    std::string_view::iterator tail = input.begin() + start,
            end = endpoint == std::string_view::npos ?
            input.end() : input.begin() + endpoint,
            head = end;
    for (; tail < end; ++tail) {
        if (isgraph(*tail)) {
            if (head == end) {
                head = tail;
            }
        } else {
            if (head != end) {
                if (!handler(head, tail)) {
                    return;
                }
                head = end;
            }
        }
    }
    handler(head, tail);
}
