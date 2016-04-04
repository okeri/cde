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

#include <iostream>
#include <string.h>
#include <sys/stat.h>
#include <dirent.h>
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
    static std::string exts[] = {".h", ".hh", ".hpp", ".hxx", ".h++",""};
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
    const auto &it = end(*path) - 1;
    if (*it == SEPARATOR) {
        path->erase(it);
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


// TODO: need some unittests for functions like this one
std::string basenameNoExt(const std::string &filename) {
    size_t start = filename.rfind(SEPARATOR);
    size_t end = filename.rfind(".");
    if (start != std::string::npos &&
        end != std::string::npos) {
        return filename.substr(start + 1, end - start - 1);
    }
    return "";
}


std::string extension(const std::string &filename) {
    size_t pos = filename.rfind(".");
    if (pos != std::string::npos) {
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


// TODO: read by 1 byte is always bad idea
const char *findLineInFile(const std::string &filename, uint32_t position) {
    std::ifstream is(filename);
    if (is) {
        char *result;
        static char buffer[256] = {0};
        is.seekg(position - 1, is.beg);
        if (is) {
            is.get(buffer[0]);
        }
        while (buffer[0] != '\n' && is) {
            is.seekg(-2, is.cur);
            if (is) {
                is.get(buffer[0]);
            }
        }
        is.getline(buffer, sizeof(buffer));
        result = buffer;
        while (!isgraph(*result)) {
            result++;
        }
        if (strlen(result) > MAX_DISP_LEN) {
            strcpy(result + MAX_DISP_LEN - 4, "...");
        }
        return result;
    } else {
        return "cannot open file";
    }
}


void mkdir(const std::string& path) {
    ::mkdir(path.c_str(), 0700);

}

/** remove ugly ../../ from path */
std::string purify(const std::string &path) {
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

}  // namespace
