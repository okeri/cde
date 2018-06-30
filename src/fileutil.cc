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
#include <filesystem>
#include <iostream>
#include <fstream>

#include "fileutil.h"

// TODO: eliminate fileutl and replace it with small
// amount of support functions because of std::filesystem

namespace {

bool endsWithLow(std::string_view str, std::string_view end) {
    size_t len = end.length();
    if (str.length() < len) {
        return false;
    }

    for (size_t i = 0, u = str.length() - len; i < len; ++i, ++u) {
        if (end[i] != tolower(str[u])) {
            return false;
        }
    }
    return true;
}

bool isSource(std::string_view path) {
    static std::string exts[] = {".c", ".cc", ".cpp", ".cxx", ".c++", ".cu" ,""};
    for (int i = 0; exts[i] != ""; ++i) {
        if (endsWithLow(path, exts[i])) {
            return true;
        }
    }
    return false;
}


}  // namespace

namespace [[depricated]] fileutil {

namespace fs = std::filesystem;

bool endsWith(std::string_view str, std::string_view end,
              const char prev) {
    size_t len = end.length();
    if (prev == 0) {
        if (str.length() < len) {
            return false;
        }
    } else if (str.length() != len) {
        if (str.length() < len + 1) {
            return false;
        } else if (str[str.length() - len - 1] != prev) {
            return false;
        }
    }

    for (size_t i = 0, u = str.length() - len; i < len; ++i, ++u) {
        if (end[i] != str[u]) {
            return false;
        }
    }
    return true;
}

bool isHeader(std::string_view path) {
    static std::string exts[] = {".h", ".hh", ".hpp", ".hxx", ".h++", ""};
    if (path.find(".") == std::string::npos) {
        return true;
    }
    for (int i = 0; exts[i] != ""; ++i) {
        if (endsWithLow(path, exts[i])) {
            return true;
        }
    }
    return false;
}

void deleteTrailingSep(std::string *path) {
    auto it = std::end(*path) - 1;
    const auto &begin = std::begin(*path);
    while (*it == SEPARATOR && it != begin) {
        path->erase(it);
        --it;
    }
}

void addTrailingSep(std::string *path) {
    const auto &it = end(*path) - 1;
    if (*it != SEPARATOR) {
        *path += SEPARATOR;
    }
}

std::string dirUp(std::string_view path) {
    size_t len = path.length();
    if (len > 1) {
        size_t pos = path.rfind(SEPARATOR, path[len - 1] == SEPARATOR ?
                                len - 2 : std::string::npos);
        if (pos != std::string::npos) {
            return std::string(path.substr(0, pos + 1));
        }
    }
    return "";
}

std::string basenameNoExt(std::string_view filename) {
    size_t start = filename.rfind(SEPARATOR);
    size_t end = filename.rfind(".");
    if (start != std::string::npos) {
        start++;
    } else {
        start = 0;
    }
    return std::string(filename.substr(
        start, end != std::string::npos ? end - start : end));
}

std::string extension(std::string_view filename) {
    size_t sep = filename.rfind(SEPARATOR);
    size_t pos = filename.rfind(".");
    if ((sep < pos || sep == std::string::npos) &&
        pos != std::string::npos) {
        return std::string(filename.substr(pos));
    }
    return "";
}

uint32_t fileTime(std::string_view filename) {
    std::error_code ec;
    auto result = fs::last_write_time(filename, ec);
    if (!ec) {
        return std::chrono::system_clock::to_time_t(result);
    }
    return INVALID;
}

bool fileExists(std::string_view filename) {
    return fs::exists(filename);
}

void collectFiles(std::string_view path,
                  std::forward_list<std::string> *files, bool checkExt) {
    if (fs::exists(path)) {
        for (const auto& entry: fs::directory_iterator(path)) {
            auto name = entry.path().filename().string();
            auto relative = entry.path().string();
            if (!entry.is_directory()) {
                if (!checkExt || isSource(name)) {
                    files->push_front(relative);
                }
            } else if (name != ".git" && name != ".svn") {
                collectFiles(relative, files, checkExt);
            }
        }
    }
}

const char *findLineInFile(const std::string &filename, uint32_t position) {
    static char buffer[4096];
    std::ifstream is(filename);
    if (is) {
        // find beginning of line
        position--;
        bool sv = position > sizeof(buffer);
        if (sv) {
            is.seekg(position - sizeof(buffer), is.beg);
        }
        size_t read = is.readsome(buffer, sizeof(buffer));
        char *tok = buffer - 1 + (sv ? read : position);
        while (*tok != '\n' && tok >= buffer) {
            --tok;
        }

        // first newline is farther 4096 bytes ??!
        if (sv && *tok != '\n') {
            return "cannot parse file";
        }

        // if buffer is full, save newline->endofbuffer and read other data
        if (read == sizeof(buffer)) {
            size_t start = read + buffer - tok;
            memmove(buffer, tok, start);
            read = is.readsome(buffer + start, sizeof(buffer) - start);
            tok = buffer;
        }

        // trim left
        while (!isgraph(*tok)) {
            tok++;
        }

        // trim right
        for (char *et = tok; *et; ++et) {
            if (*et == '\n') {
                *et = 0;
                break;
            }
        }
        if (strlen(tok) > MAX_DISP_LEN) {
            strcpy(tok + MAX_DISP_LEN - 4, "...");
        }
        return tok;

    } else {
        return "cannot open file";
    }
}


void mkdir(std::string_view path) {
    std::error_code ec;
    fs::create_directories(path, ec);
}


/** remove ugly ../../ from path */
std::string purify(std::string_view path) {
    // TODO: Windows version ?
    std::string ret(path);
    size_t tokd, tok = ret.rfind("/..");
    while (tok != std::string::npos) {
        size_t lev = 0;
        tokd = ret.rfind(SEPARATOR, tok - 1);
        while (tokd != std::string::npos && ret[tokd + 1] == '.'
               && ret[tokd + 2] == '.') {
            lev++;
            tokd = ret.rfind(SEPARATOR, tokd - 1);
        }

        while (tokd != std::string::npos && lev > 0) {
            tokd = ret.rfind(SEPARATOR, tokd - 1);
            lev--;
        }

        if (tokd == std::string::npos) {
            return std::string(path);
        }
        ret = ret.erase(tokd, tok - tokd + 3);
        tok = ret.rfind("/..");
    }
    return ret;
}

}  // namespace fileutil
