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

#include <unordered_map>
#include "cdeproject.h"

class CDE {
    string storePath_;
    bool pch_;
    unordered_map<string, CDEProject> projects_;

    inline CDEProject& getProject(const string &path) {
        return projects_.find(path)->second;
    }

    CDEProject& getProjectByFilename(const string &filename) {
        const auto& end = projects_.end();
        const auto& found =  find_if(
            projects_.begin(), end,
            [filename] (const pair<const string, CDEProject> &p) {
                return p.second.fileInProject(filename);
            });

        if  (found != end) {
            return found->second;
        }

        string root = CDEProject::findProjectRoot(
            fileutil::dirUp(filename));
        fileutil::deleteTrailingSep(&root);

        const auto& fsfound = projects_.find(root);
        if (fsfound != end) {
            return fsfound->second;
        }

        return projects_.emplace(piecewise_construct, forward_as_tuple(root),
                                 forward_as_tuple(root, storePath_, pch_))
                .first->second;
    }

  public:
    CDE(const string &store, bool pch)
            : storePath_(store), pch_(pch) {
    }

    inline void update(const string &projectpath,
                       const string &filename) {
        if (filename != "") {
            getProject(projectpath).updateProjectFile(filename, 0, false);
        } else {
            getProject(projectpath).scanProject();
        }
    }

    inline void definition(const string &projectpath, const string &filename,
                           uint32_t pos, size_t unsavedSize,
                           bool forceReparce) {
        getProject(projectpath).definition(filename, pos, unsavedSize,
                                           forceReparse);
    }

    inline void references(const string &projectpath, const string &filename,
                           uint32_t pos, size_t unsavedSize,
                           bool forceReparce) {
        getProject(projectpath).references(filename, pos, unsavedSize,
                                           forceReparse);
    }

    inline void completion(const string &projectpath, const string &filename,
                           const string &prefix, uint32_t line, uint32_t column,
                           size_t unsavedSize) {
        getProject(projectpath).completion(filename, prefix, line, column,
                                           unsavedSize);
    }

    // this will be called BEFORE ack in case we are opening file by cde
    // that's why we searching file in index first
    inline void findfile(const string &projectpath, const string &from,
                         const string &filename) {
        if (filename == "") {
            getProject(projectpath).swapSrcHdr(from);
        } else {
            getProject(projectpath).findfile(filename, from);
        }
    }

    inline void ack(const string &filename) {
        getProjectByFilename(filename).acknowledge(filename);
    }
};
