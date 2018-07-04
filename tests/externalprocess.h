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

#pragma once

#include <unistd.h>
#include <sys/wait.h>
#include <memory>

class ExternalProcess {
    int rfds_[2];
    int wfds_[2];
    size_t bufSize_;
    char *buffer_;
    FILE *rpipe_;
    FILE *wpipe_;

  public:
    ExternalProcess() : bufSize_(260) {
        buffer_ = static_cast<char*>(malloc(bufSize_));
        buffer_[0] = 0;
    }
    ~ExternalProcess() {
        close();
        if (bufSize_) {
            free(buffer_);
        }
    }
    void close() {
        ::wait(NULL);
        fclose(rpipe_);
        fclose(wpipe_);
        ::close(rfds_[0]);
        ::close(wfds_[1]);
    }
    bool open(const char *file, const char *args) {
        if (pipe(rfds_) || pipe(wfds_)) {
            return false;
        }
        if (fork() == 0) {
            dup2(wfds_[0], STDIN_FILENO);
            dup2(rfds_[1], STDOUT_FILENO);
            ::close(rfds_[0]);
            ::close(rfds_[1]);
            ::close(wfds_[0]);
            ::close(wfds_[1]);
            if (execl(file, args, NULL) == -1) {
                return false;
            }
        }
        ::close(wfds_[0]);
        ::close(rfds_[1]);

        wpipe_ = fdopen(wfds_[1], "w");
        rpipe_ = fdopen(rfds_[0], "r");
        return (rpipe_ != NULL && wpipe_ != NULL);
    }
    void send(const std::string &data) {
        fwrite(data.c_str(), 1, data.length(), wpipe_);
        fflush(wpipe_);
    }
    const char* recv() {
        if (getline(&buffer_, &bufSize_, rpipe_) != -1) {
            return buffer_;
        }
        return "error reading pipe";
    }
    static std::string selfPath() {
        char buff[260];
        ssize_t len = ::readlink("/proc/self/exe", buff, sizeof(buff) - 1);
        if (len != -1) {
            buff[len] = '\0';
            return std::string(buff);
        }
        return ".";
    }
};
