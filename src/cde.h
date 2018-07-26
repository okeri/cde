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

#pragma once

#include "cdeproject.h"

class CDE {
    std::string storePath_;
    std::unordered_map<std::string, CDEProject> projects_;
    bool nocache_;
    bool pch_;

  private:
    CDEProject& getProject(const std::string &path,
                           const std::string &filename) {
        if (const auto &found = projects_.find(path);
            found != projects_.end()) {
            return found->second;
        } else {
            ack(filename);
            return projects_.find(path)->second;
        }
    }

    CDEProject& getProjectByFilename(const std::string &filename) {
        const auto& end = projects_.end();

        if  (const auto& found = find_if(
                 projects_.begin(), end,
                 [filename] (const auto &p) {
                     return p.second.fileInProject(filename);
                 });
             found != end) {
            return found->second;
        }

        std::string root = CDEProject::findProjectRoot(
            fileutil::dirUp(filename));

        if (const auto& fsfound = projects_.find(root);
            fsfound != end) {
            return fsfound->second;
        }

        return projects_.emplace(
            std::piecewise_construct,
            std::forward_as_tuple(root),
            std::forward_as_tuple(root, storePath_,
                                  nocache_, pch_))
                .first->second;
    }

  public:
    CDE(const std::string &store, bool nocache, bool pch)
            : storePath_(store), nocache_(nocache) ,pch_(pch) {
    }

    void update(const std::string &projectpath) {
        getProject(projectpath, "").scanProject();
    }

    void check(const std::string &projectpath,
               const std::string &filename) {
        getProject(projectpath, filename).check(filename);
    }

    void definition(const std::string &projectpath,
                    const std::string &filename, uint32_t pos) {
        getProject(projectpath, filename).definition(filename, pos);
    }

    void info(const std::string &projectpath,
              const std::string &filename, uint32_t pos) {
        getProject(projectpath, filename).info(filename, pos);
    }

    void references(const std::string &projectpath,
                    const std::string &filename, uint32_t pos) {
        getProject(projectpath, filename).references(filename, pos);
    }

    void completion(const std::string &projectpath,
                    const std::string &filename,
                    const std::string &prefix, uint32_t line,
                    uint32_t column) {
        getProject(projectpath, filename).completion(
            filename, prefix, line, column);
    }

    // this will be called BEFORE ack in case we are opening file by cde
    // that's why we searching file in index first
    void findfile(const std::string &projectpath,
                  const std::string &from, const std::string &filename) {
        if (filename == "") {
            getProject(projectpath, from).swapSrcHdr(from);
        } else {
            getProject(projectpath, from).findfile(filename, from);
        }
    }

    void ack(const std::string &filename) {
        getProjectByFilename(filename).acknowledge(filename);
    }
};
