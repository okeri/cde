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

#ifdef _WIN32
#include <windows.h>
#else
#include <sys/stat.h>
#include <dirent.h>
#endif

#include <iostream>
#include <fstream>

#include "fileutil.h"

namespace fileutil {

bool endsWith(const std::string &str, const std::string &end) {
    size_t len = end.length();
    if (str.length() < len) {
        return false;
    }

    for (size_t i = 0, u = str.length() - len; i < len; ++i, ++u) {
        if (end[i] != str[u]) {
            return false;
        }
    }
    return true;
}

static bool endsWithLow(const std::string &str, const std::string &end) {
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

bool isHeader(const std::string &path) {
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

static bool isSource(const std::string &path) {
    static std::string exts[] = {".c", ".cc", ".cpp", ".cxx", ".c++", ""};
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

std::string dirUp(const std::string &path) {
    size_t len = path.length();
    if (len > 1) {
        size_t pos = path.rfind(SEPARATOR, path[len - 1] == SEPARATOR ?
                                len - 2 : std::string::npos);
        if (pos != std::string::npos) {
            return path.substr(0, pos + 1);
        }
    }
    return "";
}

std::string basenameNoExt(const std::string &filename) {
    size_t start = filename.rfind(SEPARATOR);
    size_t end = filename.rfind(".");
    if (start != std::string::npos) {
        start++;
    } else {
        start = 0;
    }
    return filename.substr(start, end != std::string::npos ?
                           end - start : end);
}

std::string extension(const std::string &filename) {
    size_t sep = filename.rfind(SEPARATOR);
    size_t pos = filename.rfind(".");
    if ((sep < pos || sep == std::string::npos) &&
        pos != std::string::npos) {
        return filename.substr(pos);
    }
    return "";
}

uint32_t fileTime(const std::string &filename) {
    struct stat st;
    if (stat(filename.c_str(), &st) != -1) {
        return st.st_mtim.tv_sec;
    }
    return INVALID;
}

bool fileExists(const std::string &filename) {
    struct stat st;
    return stat(filename.c_str(), &st) != -1 && S_ISREG(st.st_mode);
}

// TODO: Windows version ?
void collectFiles(const std::string &path,
                  std::forward_list<std::string> *files, bool checkExt) {
    struct dirent *dirent;
    DIR *dir;
    if ((dir = opendir(path.c_str())) != NULL) {
        while ((dirent = readdir(dir)) != NULL) {
            if (strcmp(dirent->d_name, ".") &&
                strcmp(dirent->d_name, "..") &&
                strcmp(dirent->d_name, ".git") &&
                strcmp(dirent->d_name, ".svn")) {
                std::string name;
                if (path == ".") {
                    name = dirent->d_name;
                } else {
                    name = path + SEPARATOR + dirent->d_name;
                }
                if (dirent->d_type != DT_DIR) {
                    if (!checkExt || isSource(name)) {
                        files->push_front(name);
                    }
                } else {
                    collectFiles(name, files);
                }
            }
        }
        closedir(dir);
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


void mkdir(const std::string& path) {
    ::mkdir(path.c_str(), 0700);
}

/** remove ugly ../../ from path */
std::string purify(const std::string &path) {
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
            return path;
        }
        ret = ret.erase(tokd, tok - tokd + 3);
        tok = ret.rfind("/..");
    }
    return ret;
}

}  // namespace fileutil
