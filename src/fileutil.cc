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
#include <filesystem>
#include <iostream>
#include <fstream>
#include <stack>
#include <optional>

#include "fileutil.h"

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
    constexpr std::string_view exts[] = {
        ".c", ".cc", ".cpp", ".cxx", ".c++", ".cu", ""};
    for (int i = 0; exts[i] != ""; ++i) {
        if (endsWithLow(path, exts[i])) {
            return true;
        }
    }
    return false;
}

}  // namespace

namespace fileutil {

namespace fs = std::filesystem;

bool hasTail(std::string_view str, std::string_view tail) {
    size_t len = tail.length();
    if (str.length() < len) {
        return false;
    }
    for (size_t i = 0, u = str.length() - len; i < len; ++i, ++u) {
        if (tail[i] != str[u]) {
            return false;
        }
    }
    return true;
}

std::string join(std::string_view first, std::string_view second) {
    return (fs::path(first) / fs::path(second)).string();
}

std::string joinp(std::string_view first, std::string_view second) {
    return (fs::path(purify(first)) / fs::path(purify(second))).string();
}

// reverted from lexically_normal because the last has strange behaviour
std::string purify(std::string_view path) {
    std::string ret(path);

    size_t tokd, tok = ret.rfind("/..");
    while (tok != std::string::npos) {
        size_t lev = 0;
        tokd = ret.rfind('/', tok - 1);
        while (tokd != std::string::npos && ret[tokd + 1] == '.' &&
               ret[tokd + 2] == '.') {
            lev++;
            tokd = ret.rfind('/', tokd - 1);
        }

        while (tokd != std::string::npos && lev > 0) {
            tokd = ret.rfind('/', tokd - 1);
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

std::string dirUp(std::string_view path) {
    return fs::path(path).parent_path().string();
}

std::string basenameNoExt(std::string_view filename) {
    auto basename = fs::path(filename).filename().string();
    size_t end = basename.rfind(".");
    return basename.substr(0, end);
}

std::string extension(std::string_view filename) {
    return fs::path(filename).extension();
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

void collectFiles(std::string_view path, std::forward_list<std::string>* files,
    bool checkExt) {
    if (fs::exists(path)) {
        for (const auto& entry : fs::directory_iterator(path)) {
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

enum { MaxDisplayLines = 12 };

const char* extractPos(char* buffer, size_t len, size_t size, uint32_t skip) {
    if (len == 0) {
        // this is workaround only for displaying variable initializers
        const char* openings[] = {"//", "/*", "\"", "'", "(", "{", nullptr};
        const char* closings[] = {"\n", "*/", "\"", "'", ")", "}", nullptr};
        const char* finalizators[] = {";", ",", nullptr};

        std::stack<unsigned> levels;

        auto equaln = [](const char* a, const char* b, size_t max) {
            size_t ei = 0;
            for (; b[ei] && a[ei] && ei < max && a[ei] == b[ei]; ++ei) {
            }
            return b[ei] == 0;
        };

        auto scan = [&equaln](const char* whence, size_t max,
                        const char** items) -> std::optional<unsigned> {
            for (auto element = 0; items[element] != nullptr; ++element) {
                if (equaln(whence, items[element], max)) {
                    return element;
                }
            }
            return std::nullopt;
        };

        char* head;
        auto end = buffer + size;
        for (head = buffer + skip; head < end; ++head) {
            auto rest = end - buffer;
            if (levels.empty()) {
                if (scan(head, rest, finalizators)) {
                    *head = 0;
                    len = head - buffer - 1;
                    break;
                }
            } else if (equaln(head, closings[levels.top()], rest)) {
                if (levels.top() == 1) {
                    ++head;
                }
                levels.pop();
                continue;
            }

            auto v = scan(head, rest, openings);
            if (v) {
                levels.push(*v);
                if (*v < 2) {
                    ++head;
                }
            }
        }

    } else {
        *(buffer + std::min(size - 1, len)) = '\0';
    }

    unsigned lines = 0;
    for (auto head = buffer; head < buffer + len; ++head) {
        if (*head == '\n') {
            if (++lines > MaxDisplayLines) {
                strcpy(head - 4, "...");
                break;
            }
        }
    }
    // ellipse path
    if (len > size) {
        strcpy(buffer + size - 4, "...");
    }
    return buffer;
}

const char* extractPosInString(
    std::string_view data, uint32_t start, uint32_t end, uint32_t skip) {
    static char buffer[4096];
    auto len = end != INVALID ? end - start : 0;
    auto size = std::min(
        end != INVALID ? len + 1 : data.length() - start + 1, sizeof(buffer));
    std::copy(data.data() + start, data.data() + start + size, buffer);
    return extractPos(buffer, len, size, skip);
}

const char* extractPosInFile(
    const std::string& filename, uint32_t start, uint32_t end, uint32_t skip) {
    static char buffer[4096];
    std::ifstream is(filename);
    if (is) {
        is.seekg(start, is.beg);
        size_t len = end != INVALID ? end - start : 0;
        size_t size;
        if (end != INVALID) {
            size = std::min(sizeof(buffer), len + 1);
            is.readsome(buffer, size);
        } else {
            size = is.readsome(buffer, sizeof(buffer));
        }
        return extractPos(buffer, len, size, skip);
    } else {
        return "cannot open source file";
    }
}

const char* findLineInFile(const std::string& filename, uint32_t position) {
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
        char* tok = buffer - 1 + (sv ? read : position);
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
        for (char* et = tok; *et; ++et) {
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
        return "cannot open source file";
    }
}

void mkdir(std::string_view path) {
    std::error_code ec;
    fs::create_directories(path, ec);
}

}  // namespace fileutil
