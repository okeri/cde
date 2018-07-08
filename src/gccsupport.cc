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

#include <string.h>
#include <stdlib.h>
#include <iostream>
#include <string_view>
#include "strbreak.h"
#include "gccsupport.h"
#include "fileutil.h"

GccSupport& GccSupport::inst() {
    static GccSupport instance;
    return instance;
}

void GccSupport::init(const std::string &path) {
    std::string cmd = path + " -v -E -x c++ /dev/null 2>&1";
    setenv("LANG", "C" , 1);
    if (auto pipe = popen(cmd.c_str(), "r"); pipe) {
        char buffer[0xfff];
        std::string result;
        while (fgets(buffer, sizeof(buffer), pipe)) {
            result += buffer;
        }
        constexpr std::string_view startTag = "#include <...> search starts here:";
        if (auto start = result.find(startTag);
            start != std::string::npos) {
            start += startTag.length();

            if (auto end = result.find("End of search list.", start);
                end != std::string::npos) {
                std::unordered_set<std::string> &includes = inst().includes_;
                strBreak(result, [&includes] (auto begin, auto end) {
                        includes.emplace(std::string("-I") +
                                         fileutil::purify(
                                             std::string(begin, end)));
                        return true;
                    }, start, end);
            } else {
                std::cout << "(message \"" << path
                          << " returned garbage\")" << std::endl;
            }
        } else {
            std::cout << "(message \"" << path
                      << " returned garbage\")" << std::endl;
        }
        pclose(pipe);
    } else {
        std::cout << "(message \"cannot exec " << path << "\")" << std::endl;
    }
}

std::unordered_set<std::string> &GccSupport::includes() {
    return inst().includes_;
}
