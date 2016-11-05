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

#include <future>
#include "emacs.h"
//#include "cde.h"

//CDE _cde;

std::future<void> f;

emacs_value update(emacs_env *env, ptrdiff_t nargs,
                   emacs_value args[], void *data) {
    return env->intern(env, "t");
}

emacs_value check(emacs_env *env, ptrdiff_t nargs,
                  emacs_value args[], void *data) {
    return env->intern(env, "t");
}

emacs_value definition(emacs_env *env, ptrdiff_t nargs,
                       emacs_value args[], void *data) {
    return env->intern(env, "t");
}

emacs_value references(emacs_env *env, ptrdiff_t nargs,
                       emacs_value args[], void *data) {
    return env->intern(env, "t");
}

emacs_value completion(emacs_env *env, ptrdiff_t nargs,
                       emacs_value args[], void *data) {
    return env->intern(env, "t");
}

emacs_value findfile(emacs_env *env, ptrdiff_t nargs,
                     emacs_value args[], void *data) {
    return env->intern(env, "t");
}

emacs_value ack(emacs_env *env, ptrdiff_t nargs,
                emacs_value args[], void *data) {
    Emacs::init(env);
    Emacs::funcall("message", "starting test");
    //    return Emacs::funcall("cde--test");
    f = std::async([](){
            std::this_thread::sleep_for(std::chrono::milliseconds(3000));
            Emacs::funcall("message", "ack done !!!");
        });

    Emacs::funcall("message", "return from cde--test !!!");
    return env->intern(env, "t");
}


/* Declare mandatory GPL symbol.*/
int plugin_is_GPL_compatible;

int emacs_module_init (struct emacs_runtime *ert) {
    emacs_env *env = ert->get_environment(ert);
    Emacs::init(env);
    Emacs::bindFunction("cde--update",
                        env->make_function(
                            env, 1, 2, update,
                            "Reparse unit(s)", NULL));

    Emacs::bindFunction("cde--check",
                        env->make_function(
                            env, 2, 2, check,
                            "Syntax check of file", NULL));

    Emacs::bindFunction("cde--definition",
                        env->make_function(
                            env, 3, 3, definition,
                            "find symbol definition", NULL));

    Emacs::bindFunction("cde--references",
                        env->make_function(
                            env, 3, 3, references,
                            "Get symbol references", NULL));

    Emacs::bindFunction("cde--completion",
                        env->make_function(
                            env, 5, 5, completion,
                            "Code completion", NULL));

    Emacs::bindFunction("cde--findfile",
                        env->make_function(
                            env, 2, 3, findfile,
                            "Code completion", NULL));

    Emacs::bindFunction("cde--ack",
                        env->make_function(
                            env, 1, 1, ack,
                            "send ack", NULL));

    Emacs::funcall("provide", "libcde", true);
    return 0;
}
