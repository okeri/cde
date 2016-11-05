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

#include "emacs.h"

Emacs::Emacs() : lock_(ATOMIC_FLAG_INIT) {
}

Emacs& Emacs::instance() {
    static Emacs instance;
    return instance;
}

void Emacs::init(emacs_env *env) {
    instance().env_ = env;
}

emacs_value Emacs::makeval(const std::string &val) {
    emacs_env *env = instance().env_;
    return env->make_string(env, val.c_str(), val.length());
}

emacs_value Emacs::makeval(uint32_t val) {
    emacs_env *env = instance().env_;
    return env->make_integer(env, val);
}

emacs_value Emacs::function(const std::string &name) {
    auto found = functions_.find(name);
    if (found != functions_.end()) {
        return found->second;
    }
    return functions_.emplace(name, env_->intern(
        env_, name.c_str())).first->second;
}

emacs_value Emacs::funcall(const std::string &name) {
    Emacs &inst = instance();
    return inst.env_->funcall(inst.env_, inst.function(name), 0, nullptr);
}

emacs_value Emacs::funcall(const std::string &name, const std::string &value,
                           bool symbol) {
    Emacs &inst = instance();
    emacs_value val;
    if (symbol) {
        val = inst.env_->intern(inst.env_,
                                value.c_str());
    } else {
        val = inst.env_->make_string(inst.env_,
                                     value.c_str(), value.length());
    }
    return inst.env_->funcall(inst.env_, inst.function(name), 1, &val);
}

emacs_value Emacs::funcall(const std::string &name, emacs_value v) {
    Emacs &inst = instance();
    return inst.env_->funcall(inst.env_, inst.function(name), 1, &v);
}

emacs_value Emacs::funcall(const std::string &name, emacs_value v1,
                           emacs_value v2) {
    Emacs &inst = instance();
    emacs_value args[] = {v1, v2};
    return inst.env_->funcall(inst.env_, inst.function(name), 2, args);
}

emacs_value Emacs::funcall(const std::string &name, ptrdiff_t num,
                           emacs_value args[]) {
    Emacs &inst = instance();
    return inst.env_->funcall(inst.env_, inst.function(name), num, args);
}

emacs_value Emacs::bindFunction(const std::string &name, emacs_value v) {
    Emacs &inst = instance();
    emacs_value args[] = {inst.env_->intern(inst.env_, name.c_str()), v};
    return inst.env_->funcall(inst.env_, inst.function("fset"), 2,  args);
}

bool Emacs::lock() {
    return !instance().lock_.test_and_set(std::memory_order_acquire);
}

void Emacs::release() {
    instance().lock_.clear(std::memory_order_release);
}
