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

#include "cde.h"
#include "gccsupport.h"

#define DEFAULT_PATH         "/tmp"
#define DEFAULT_GCC_PATH     "gcc"

void stringToArgv(const string& inp, vector<string> *v) {
    strBreak(inp, [v](const char* head, size_t len) {
            v->emplace_back(head, len);
            return true;
        });
}

int main(int argc, char *argv[]) {
    string path(DEFAULT_PATH);
    string gccpath(DEFAULT_GCC_PATH);
    bool pch = false;

    for (unsigned i = 1; i < argc; ++i) {
        size_t len = strlen(argv[i]);
        if (len > 1 && argv[i][0] == '-') {
            switch (argv[i][1]) {
                case 'C':
                    if (len > 2) {
                        path = argv[i] + 2;
                    }
                    break;

                case 'P':
                    pch = true;
                    break;

                case 'G':
                    if (len > 2) {
                        gccpath = argv[i] + 2;
                    }
                    break;
            }
        }
    }

    if (gccpath != "n" && !gccpath.empty()) {
        gccSupport::init(gccpath);
    }

    fileutil::deleteTrailingSep(&path);

    CDE cde(path, pch);
    string command, last;
    vector<string> commands(8);

    // Looks like we do not need error handling here :P
    while (getline(cin, command)) {
        commands.resize(0);
        stringToArgv(command, &commands);
        unsigned count = commands.size();
        if (count > 0) {
            switch (commands[0][0]) {
                case 'A':
                    if (count > 1 && command != last) {
                        cde.ack(commands[1]);
                    }
                    break;

                case 'B':
                    if (count > 4) {
                        cde.check(commands[1], commands[2],
                                  stoi(commands[3]), stoi(commands[4]));
                    }
                    break;

                case 'C':
                    if (count > 5) {
                        unsigned prefixOfs = count - 4;
                        cde.completion(commands[1], commands[2], prefixOfs ==
                                       2 ? "" : commands[prefixOfs],
                                       stoi(commands[prefixOfs + 1]),
                                       stoi(commands[prefixOfs + 2]) + 1,
                                       stoi(commands[prefixOfs + 3]));
                    }
                    break;

                case 'D':
                    if (count > 3) {
                        cde.definition(commands[1], commands[2],
                                       stoi(commands[3]) - 1,
                                       count > 5 ? stoi(commands[5]) : 0,
                                       count > 5 ? stoi(commands[4]) : 0);
                    }
                    break;

                case 'F':
                    if (count > 2) {
                        cde.findfile(commands[1], commands[2],
                                     count > 3 ? commands[3] : "");
                    }
                    break;

                case 'R':
                    if (count > 3) {
                        cde.references(commands[1], commands[2],
                                       stoi(commands[3]) - 1,
                                       count > 5 ? stoi(commands[5]) : 0,
                                       count > 5 ? stoi(commands[4]) : 0);
                    }
                    break;

                case 'U':
                    if (count > 1) {
                        cde.update(commands[1], (count > 2) ?
                                   commands[2] : "");
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
