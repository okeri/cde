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

#include <string.h>
#include <iostream>
#include "gccsupport.h"


gccSupport& gccSupport::inst() {
    static gccSupport instance;
    return instance;
}

void gccSupport::init(const std::string &path) {
    std::string cmd = path + " -v -E -x c++ /dev/null";

    FILE *pipe = popen(cmd.c_str(), "r");
    if (pipe) {
        char buffer[0xfff];
        std::string result;
        while (!feof(pipe)) {
            if (fgets(buffer, sizeof(buffer), pipe))
                result += buffer;
        }
        size_t start = result.find("#include <...> search starts here:");
        if (start != std::string::npos) {
            size_t end = result.find("End of search list.");
            if (end != std::string::npos) {
                std::unordered_set<std::string> &includes = inst().includes_;
                strBreak(result, [&includes, start, end]
                         (const char* head, size_t len) {
                             includes.emplace(head, len);
                        return true;
                    });
            } else {
                std::cout << "(message \"gcc returned garbage\")" << std::endl;
            }
        } else {
            std::cout << "(message \"gcc returned garbage\")" << std::endl;
        }
        pclose(pipe);
    } else {
        std::cout << "(message \"cannot exec " << path << "\")" << std::endl;
    }
}


std::unordered_set<std::string> &gccSupport::includes() {
    return inst().includes_;
}
