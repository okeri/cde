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

#if defined(__linux__) || defined(__FreeBSD__) || defined(__APPLE__)
#include <signal.h>
#endif

#include <charconv>

#include "cde.h"
#include "config.h"
#include "gccsupport.h"
#include "emacsmapper.h"

int main(int argc, char* argv[]) {
    std::string path(DEFAULT_PATH);
    std::string gccpath(DEFAULT_GCC_PATH);
    bool nocache = false;
    for (auto i = 0; i < argc; ++i) {
        size_t len = strlen(argv[i]);
        if (len > 1 && argv[i][0] == '-') {
            switch (argv[i][1]) {
                case 'C':
                    if (len > 2) {
                        path = argv[i] + 2;
                    } else if (len == 2 && argv[i][1] == 'n') {
                        nocache = true;
                    }
                    break;

                case 'G':
                    if (len > 2) {
                        gccpath = argv[i] + 2;
                    }
                    break;
            }
        }
    }

#if defined(__linux__) || defined(__FreeBSD__) || defined(__APPLE__)
    signal(SIGPIPE, SIG_IGN);
    signal(SIGHUP, SIG_IGN);
#endif

    std::ios_base::sync_with_stdio(false);

    if (gccpath != "n" && !gccpath.empty()) {
        GccSupport::init(gccpath);
    }

    fileutil::mkdir(path);

    CDE cde(path, nocache);
    std::string command, last;
    std::vector<std::string> commands(8);

    auto cast = [](std::string_view str) -> uint32_t {
        uint32_t result;
        std::from_chars(str.begin(), str.end(), result);
        return result;
    };

    // Looks like we do not need error handling here :P
    while (getline(std::cin, command)) {
        commands.resize(0);
        strBreak(command, [&commands](auto begin, auto end) {
            commands.emplace_back(begin, end);
            return true;
        });

        if (unsigned count = commands.size(); count > 0) {
            switch (commands[0][0]) {
                case 'A':
                    if (count > 1 && command != last) {
                        cde.ack(commands[1]);
                    }
                    break;

                case 'B':
                    if (count == 3) {
                        cde.check(commands[1], commands[2]);
                    }
                    break;

                case 'C':
                    if (count > 4) {
                        unsigned prefixOfs = count - 3;
                        cde.completion(commands[1], commands[2],
                            prefixOfs == 2 ? "" : commands[prefixOfs],
                            cast(commands[prefixOfs + 1]),
                            cast(commands[prefixOfs + 2]) + 1);
                    }
                    break;

                case 'D':
                    if (count == 4 && !commands[3].empty()) {
                        cde.definition(
                            commands[1], commands[2], cast(commands[3]) - 1);
                    }
                    break;

                case 'F':
                    if (count > 2) {
                        cde.findfile(commands[1], commands[2],
                            count > 3 ? commands[3] : "");
                    }
                    break;

                case 'I':
                    if (count == 4 && !commands[3].empty()) {
                        cde.info(
                            commands[1], commands[2], cast(commands[3]) - 1);
                    }
                    break;

                case 'M':
                    if (count == 3) {
                        EmacsMapper::map(commands[1], cast(commands[2]));
                    } else {
                        EmacsMapper::unmap(commands[1]);
                    }
                    break;

                case 'R':
                    if (count == 4 && !commands[3].empty()) {
                        cde.references(
                            commands[1], commands[2], cast(commands[3]) - 1);
                    }
                    break;

                case 'U':
                    if (count == 2) {
                        cde.update(commands[1]);
                    }
                    break;

                case 'Q':
                    return 0;
            }
        }
        last = command;
    }
    return -1;
}
