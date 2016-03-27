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

#include <string.h>
#include <cstdint>
#include <string>
#include <vector>
#include <map>
#include <unordered_set>
#include <algorithm>
#include "fileutil.h"
#include "strbreak.h"

#define DF_NONE              0x0
#define DF_FWD               0x1

using namespace std;

#pragma pack(push, 1)

struct CI_DATA {
    uint32_t file;
    uint32_t pos;
    uint32_t refline : 30;
    uint8_t flags : 2;
};

struct CI_KEY {
    uint32_t file;
    uint32_t pos;
    bool operator<(const CI_KEY &rhs) const {
        if (file == rhs.file) {
            return (pos < rhs.pos);
        }
        else {
            return (file < rhs.file);
        }
    }

    bool operator==(const CI_KEY &rhs) const {
        return (pos == rhs.pos && file == rhs.file);
    }

    bool operator!=(const CI_KEY &rhs) const {
        return !(*this == rhs);
    }

    bool operator==(const CI_DATA &rhs) const {
        return (pos == rhs.pos && file == rhs.file);
    }

    bool operator!=(const CI_DATA &rhs) const {
        return !(*this == rhs);
    }

    void swapWithData(CI_DATA *data, uint32_t line) {
        uint32_t hpos = pos, hfile = file;
        pos = data->pos;
        file = data->file;
        data->pos = hpos;
        data->file = hfile;
        data->refline = line;
    }
};

#pragma pack(pop)

class SourceInfo {
    uint32_t fileId_;
    uint32_t updated_time_;
    vector<string> args_;
    const SourceInfo *parent_;

    inline const vector<string> &args() const {
        const SourceInfo *token = this;
        while (token->parent_ != nullptr && token->args_.empty()) {
            token  = token->parent_;
        }
        return token->args_;
    }

  public:
    struct SourceInfoPacked {
        uint32_t pid;
        uint32_t updated_time;
        char filename[1];
    };

    SourceInfo(uint32_t fid, const SourceInfo *parent,
               uint32_t updated_time = 0)
            : fileId_(fid), updated_time_(updated_time),
              parent_(parent) {}

    inline uint32_t getId() const {
        return fileId_;
    }

    inline void setArgs(const string& args) {
        args_.resize(0);
        strBreak(args, [this] (const char* head, size_t len) {
                args_.emplace_back(head,len);
                return true;
            });
    }

    uint32_t fillPack(const string& filename, void *pack) const {
        size_t size(65 + filename.length());
        SourceInfoPacked *data = static_cast<SourceInfoPacked*> (
            pack);
        data->pid = parent_ != nullptr ? parent_->fileId_ : 0;
        data->updated_time = updated_time_;
        strcpy(data->filename, filename.c_str());
        return size;
    }

    void setTime(uint32_t updated_time) {
        updated_time_ = updated_time;
    }

    uint32_t time() {
        return updated_time_;
    }

    bool haveNostdinc() const {
        const vector<string> &arguments = args();
        for (const auto &s : arguments) {
            if (s == "-nostdinc") {
                return true;
            }
        }
        return false;
    }

    void fillIncludes(unordered_set<string> *includes) const {
        const vector<string> &arguments = args();
        for (const auto &s : arguments) {
            if (s.length() > 2 && s[0] == '-' && s[1]=='I') {
                includes->emplace(s.c_str() + 2, s.length() - 2);
            }
        }
    }

    void copyArgsToClangArgs(vector<const char*> *clang_args) const {
        const vector<string> &arguments = args();
        for (const auto &s : arguments) {
            clang_args->push_back(s.c_str());
        }
    }
};

typedef map<string, SourceInfo>::iterator SourceIter;

class CDEIndex {
    SourceIter root_;

  protected:
    string storePath_;

  public:
    map<CI_KEY, CI_DATA> records_;
    map<string, SourceInfo> files_;

    CDEIndex(const string& projectPath, const string& storePath)
            : storePath_(storePath) {
        string projPath = projectPath;
        root_ = files_.emplace(piecewise_construct, forward_as_tuple(projPath),
                       forward_as_tuple(0, nullptr)).first;
    }

    const SourceIter findFile(const string &filename) {
        const SourceIter &end = files_.end();
        return find_if(
            files_.begin(), end,
            [filename] (const pair<string, SourceInfo> &p) {
                return fileutil::endsWith(p.first, filename);
            });
    }

    inline const SourceIter getTUFile(const string &filename) {
        return getFile(filename, &root_->second);
    }


    const SourceIter getFile(const string &filename,
                             const SourceInfo *parent) {
        const auto &it = files_.find(filename);
        if (it != files_.end()) {
            return it;
        } else {
            uint32_t nval = files_.size() + 1;
            return files_.emplace(piecewise_construct,
                                  forward_as_tuple(filename),
                                  forward_as_tuple(nval, parent)).first;
        }
    }

    inline const SourceIter fileInfo(uint32_t fid) {
        for (auto it = begin(files_); it != end(files_); ++it) {
            if (it->second.getId() == fid) {
                return it;
            }
        }
        return files_.end();
    }

    inline const string& fileName(uint32_t fid) {
        for (const auto& it: files_) {
            if (it.second.getId() == fid) {
                return it.first;
            }
        }
        static string error("<error>");
        return error;
    }

    inline const string& projectPath() {
        return root_->first;
    }

    inline void setGlobalArgs(const string &args) {
        root_->second.setArgs(args);
    }
    virtual bool parse(const SourceIter &info, const string &unsaved,
                       bool fromCompletion, bool noTimeCheck) = 0;
    virtual void loadPCHData() = 0;
    virtual void completion(const SourceIter &info, const string &prefix,
                            uint32_t line, uint32_t column,
                            const string &unsaved) = 0;
    virtual ~CDEIndex() {
    };
};
