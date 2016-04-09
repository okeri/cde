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
#include <unordered_map>
#include <unordered_set>
#include <queue>
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

//if parents_.size == 0, sourceinfo is root
class SourceInfo {
    uint32_t fileId_;
    uint32_t updated_time_;
    string filename_;
    vector<string> args_;
    vector<const SourceInfo *> parents_;

    friend class CDEIndex;
    friend struct std::hash<SourceInfo>;

    inline const vector<string> &args() const {
        return getDominatedParent()->args_;
    }

  public:
    struct SourceInfoPacked {
        uint32_t updated_time;
        uint32_t parent_count;

        inline uint32_t *parents() {
            return reinterpret_cast<uint32_t*>(this) + 2;
        }

        inline char *filename() {
            return reinterpret_cast<char *>(
                parents() + sizeof(uint32_t) * parent_count);
        }
    };

    SourceInfo(uint32_t fid, const string& filename,
               uint32_t updated_time = 0)
            : fileId_(fid), updated_time_(updated_time),
              filename_(filename){}

    inline uint32_t getId() const {
        return fileId_;
    }

    inline const string& fileName() const {
        return filename_;
    }

    inline void setArgs(const string& args) {
        args_.resize(0);
        strBreak(args, [this] (const char* head, size_t len) {
                args_.emplace_back(head,len);
                return true;
            });
    }

    size_t fillPack(void *pack) const {
        SourceInfoPacked *data = static_cast<SourceInfoPacked*> (
            pack);
        data->parent_count = parents_.size();
        data->updated_time = updated_time_;


        uint32_t *parents  = data->parents();
        for (unsigned i = 0; i < data->parent_count; ++i) {
            parents[i] = parents_[i]->getId();
        }
        strcpy(data->filename(), filename_.c_str());
        return 65 + filename_.length() + sizeof(uint32_t) * data->parent_count;
    }

    void setTime(uint32_t updated_time) {
        updated_time_ = updated_time;
    }

    uint32_t time() const {
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
            if (s.length() > 2 && s[0] == '-' && s[1] == 'I') {
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

    inline const SourceInfo* getDominatedParent() const {
        const SourceInfo *token = this;
        while (token->parents_.size() != 0 && token->args_.empty()) {
            token  = token->parents_[0];
        }
        return token;
    }

    bool operator==(const SourceInfo& rhs) const {
        return filename_ == rhs.filename_;
    }
};

namespace std {

template <>
struct hash<CI_KEY> {
    size_t operator()(const CI_KEY& k) const {
        return hash<int>()(k.file) ^ (hash<int>()(k.pos) << 1);
    }
};

template <>
struct hash<SourceInfo> {
    size_t operator()(const SourceInfo& k) const {
        return hash<string>()(k.filename_);
    }
};

}


class CDEIndex {
    SourceInfo *root_;

    inline void eliminateRootParent(SourceInfo *si) {
        if (si->parents_.size() == 1 && si->parents_[0] == root_) {
            si->parents_.resize(0);
        }
    }

  protected:
    string storePath_;

  public:
    // TODO: records_ -> bimap ???
    // i dont want to use boost here, because of dependency
    // so it's time to think
    unordered_map<CI_KEY, CI_DATA> records_;
    // TODO: make files_  multi indexed
    unordered_set<SourceInfo> files_;

  public:
    CDEIndex(const string& projectPath, const string& storePath)
            : storePath_(storePath) {
        string projPath = projectPath;
        root_ = addInfo(0, projPath);
    }

    inline SourceInfo* addInfo(uint32_t id, const string &path,
                                     uint32_t time = 0) {
        return const_cast<SourceInfo*>(&(*files_.emplace(id, path, time).first));
    }

    /** get SourceInfo or nullptr by filename */
    SourceInfo* fileInfo(const string &filename) {
        static SourceInfo needed(0,"");
        needed.filename_ =  filename;
        auto it = files_.find(needed);
        if (it != files_.end()) {
            return const_cast<SourceInfo*>(&(*it));
        }
        return nullptr;
    }

    /** get SourceInfo or nullptr by file id */
    SourceInfo* fileInfo(uint32_t fid) {
        for (auto it = begin(files_); it != end(files_); ++it) {
            if (it->getId() == fid) {
                return const_cast<SourceInfo*>(&(*it));
            }
        }
        return nullptr;
    }

    /** find files in index  ending with filename*/
    const SourceInfo* findFile(const string &filename) {
        const auto &end = files_.end();
        auto it = find_if(
            files_.begin(), end,
            [filename] (const SourceInfo &si) {
                return fileutil::endsWith(si.fileName(), filename);
            });
        if (it != end) {
            return &(*it);
        }
        return nullptr;
    }

    /** get translation unit for current file*/
    SourceInfo* getAnyTU(const SourceInfo *info) {
        const SourceInfo *token = info;
        while (token->parents_.size() != 1 || token->parents_[0] != root_) {
            token = token->parents_.at(0);
        }
        return const_cast<SourceInfo*>(token);
    }

    /** get all translation units for current file*/
    const unordered_set<SourceInfo*> getAllTUs(SourceInfo *info) {
        unordered_set<SourceInfo *> ret;
        queue<SourceInfo *> stk;
        SourceInfo *token;
        stk.push(info);

        while (!stk.empty()) {
            token = stk.front();
            stk.pop();
            for (auto it = token->parents_.begin();
                 it != token->parents_.end(); ++it) {
                stk.push(const_cast<SourceInfo*>(*it));
                if (token->parents_.size() == 1 && token->parents_[0] == root_) {
                    ret.insert(token);
                }
            }
        }
        return ret;
    }

    /** get a file from index, or add it if files is not present in index*/
    SourceInfo * getFile(const string &filename) {
        SourceInfo *info = fileInfo(filename);
        if (info != nullptr) {
            return info;
        } else {
            info =  addInfo(files_.size() + 1, filename);
            // assume this file is TU
            info->parents_.push_back(root_);
            return info;
        }
    }

    /** set parent-child dependency*/
    inline void link(SourceInfo *info, uint32_t pid) {
        SourceInfo *psi = fileInfo(pid);
        // no checks here, because this will be called first
        // after loading project and we trust previously saved data
        if (psi != nullptr) {
            info->parents_.push_back(psi);
        }
    }

    /** set parent-child dependency*/
    inline void link(SourceInfo *file, const SourceInfo *parent) {
        eliminateRootParent(file);
        const auto &end = file->parents_.end();
        if (find(file->parents_.begin(), end, parent) == end) {
            file->parents_.push_back(parent);
        }
    }

    inline const string& fileName(uint32_t fid) {
        SourceInfo *si = fileInfo(fid);
        if (si != nullptr) {
            return si->fileName();
        }
        static string error("<error>");
        return error;
    }

    inline const string& projectPath() {
        return root_->fileName();
    }

    inline void setGlobalArgs(const string &args) {
        root_->setArgs(args);
    }
    virtual bool parse(SourceInfo *info, bool recursive) = 0;
    virtual void preprocess(SourceInfo *info) = 0;
    virtual void loadPCHData() = 0;
    virtual void completion(SourceInfo *info, const string &prefix,
                            uint32_t line, uint32_t column) = 0;
    virtual ~CDEIndex() {
    };
};
