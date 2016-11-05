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
#include <unordered_map>
#include <atomic>

#include "emacs-module.h"
#include "single.h"

class Emacs : public Single {
    emacs_env *env_;
    std::unordered_map<std::string, emacs_value> functions_;

    volatile std::atomic_flag lock_;

    emacs_value function(const std::string &name);

    static Emacs& instance();

  protected:
    Emacs();

  public:
    static void init(emacs_env *env);
    static emacs_value makeval(const std::string &val);
    static emacs_value makeval(uint32_t val);
    static emacs_value funcall(const std::string &name);
    static emacs_value funcall(const std::string &name, emacs_value v);
    static emacs_value funcall(const std::string &name, const std::string &value,
                               bool symbol = false);
    static emacs_value funcall(const std::string &name, emacs_value v1, emacs_value v2);
    static emacs_value funcall(const std::string &name, ptrdiff_t num, emacs_value args[]);
    static emacs_value bindFunction(const std::string &name, emacs_value v);
    static bool lock();
    static void release();
};
