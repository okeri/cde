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
#include <map>
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
        } else {
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


namespace std {

template <>
struct hash<CI_KEY> {
    size_t operator()(const CI_KEY& k) const {
        return hash<int>()(k.file) ^ (hash<int>()(k.pos) << 1);
    }
};

}  // namespace std

enum { ROOTID = 0 };
// assume some average project has --> 1024 files.
enum { MININDEXALLOC = 0x400 };
enum { MINPARENTNODEALLOC = 0x100 };

class SourceInfo {
    uint32_t fileId_;
    uint32_t updated_time_;
    string filename_;
    vector<string> args_;
    vector<uint32_t> parents_;

    friend class CDEIndex;

  public:
    struct SourceInfoPacked {
        uint32_t updated_time;
        uint32_t parent_count;

        inline uint32_t *parents() {
            return reinterpret_cast<uint32_t*>(this) + 2;
        }

        inline char *filename() {
            return reinterpret_cast<char *>(
                parents() + parent_count);
        }
    };

    SourceInfo(uint32_t fid, const string& filename,
               uint32_t updated_time = 0, uint32_t parentCount = 0,
               uint32_t *parents = nullptr)
            : fileId_(fid), updated_time_(updated_time),
              filename_(filename) {
        if (parentCount) {
            parents_.resize(parentCount);
            std::copy(parents, parents + parentCount, parents_.begin());
        }
    }

    inline uint32_t getId() const {
        return fileId_;
    }

    inline const string& fileName() const {
        return filename_;
    }

    inline void setArgs(const string& args) {
        args_.resize(0);
        strBreak(args, [this] (const char* head, size_t len) {
                args_.emplace_back(head, len);
                return true;
            });
    }

    size_t fillPack(void *pack, size_t bufSize) const {
        SourceInfoPacked *data = static_cast<SourceInfoPacked*>(pack);
        data->parent_count = parents_.size();
        data->updated_time = updated_time_;
        if (data->parent_count != 0) {
            std::copy(parents_.begin(), parents_.end(), data->parents());
        }
        size_t ret = 65 + sizeof(uint32_t) * data->parent_count;
        snprintf(data->filename(), bufSize - ret, "%s", filename_.c_str());
        return ret + filename_.length();
    }

    void setTime(uint32_t updated_time) {
        updated_time_ = updated_time;
    }

    uint32_t time() const {
        return updated_time_;
    }

    bool operator==(const SourceInfo& rhs) const {
        return fileId_ == rhs.fileId_;
    }
};

class CDEIndex {
    std::vector<SourceInfo> files_;
    std::map<size_t, size_t> hfilenames_;
    std::hash<std::string> hashStr;

  protected:
    string storePath_;

  public:
    unordered_map<CI_KEY, CI_DATA> records_;

  private:
    inline const SourceInfo* getDominatedParent(const SourceInfo * si) const {
        const SourceInfo *token = si;
        while (token->parents_.size() != 0 && token->args_.empty()) {
            token = &files_[token->parents_[0]];
        }
        return token;
    }

    inline const vector<string> &args(uint32_t file) const {
        return getDominatedParent(&files_[file])->args_;
    }

  protected:
    bool haveNostdinc(uint32_t file) const {
        const vector<string> &arguments = args(file);
        for (const auto &s : arguments) {
            if (s == "-nostdinc") {
                return true;
            }
        }
        return false;
    }

    void copyArgsToClangArgs(uint32_t file,
                             vector<const char*> *clang_args) const {
        const vector<string> &arguments = args(file);
        for (const auto &s : arguments) {
            clang_args->push_back(s.c_str());
        }
    }

    /** get SourceInfo or nullptr by filename */
    SourceInfo* find(const string &filename) {
        auto it = hfilenames_.find(hashStr(filename));
        return it != hfilenames_.end() ? &files_[it->second] : nullptr;
    }

    /** get SourceInfo or nullptr by file id */
    SourceInfo* find(uint32_t fid) {
        return fid < files_.size() ? &files_[fid] : nullptr;
    }


  public:
    CDEIndex(const string &projectPath, const string& storePath)
            : storePath_(storePath) {
        string projPath = projectPath;
        files_.reserve(MININDEXALLOC);
        push(0, projPath);
    }

    inline std::vector<SourceInfo>::const_iterator begin() {
        return files_.begin();
    }

    inline std::vector<SourceInfo>::const_iterator end() {
        return files_.end();
    }

    void push(uint32_t id, const string &path,
                            uint32_t time = 0, uint32_t parentCount = 0,
                            uint32_t *parents = nullptr) {
        if (id >= files_.size()) {  //  mostly prevents secont root insert
            files_.emplace_back(id, path,
                           time, parentCount, parents);
            SourceInfo *ret = &files_[files_.size() - 1];
            hfilenames_.insert(std::make_pair(hashStr(ret->filename_),
                                              ret->fileId_));
        }
    }

    /** find files in index  ending with filename*/
    uint32_t findFile(const string &filename) {
        const auto &end = files_.end();
        auto it = find_if(
            files_.begin(), end,
            [filename] (const SourceInfo &si) {
                return fileutil::endsWith(si.filename_, filename);
            });
        if (it != end) {
            return it->fileId_;
        }
        return INVALID;
    }

    /** get translation unit for current file*/
    uint32_t getAnyTU(uint32_t file) {
        uint32_t token = file;
        while (files_[token].parents_.size() > 0 &&
               files_[token].parents_[0] != ROOTID) {
            token = files_[token].parents_[0];
        }
        return token;
    }

    /** get all translation units for current file*/
    const unordered_set<uint32_t> getAllTUs(uint32_t file) {
        unordered_set<uint32_t> result;
        vector<uint32_t> nodes;
        unsigned curNode;
        nodes.reserve(MINPARENTNODEALLOC);
        nodes.push_back(files_[file].fileId_);

        for (curNode = 0; curNode < nodes.size(); ++curNode) {
            SourceInfo *token = &files_[nodes[curNode]];
            for (auto it = token->parents_.begin();
                 it != token->parents_.end(); ++it) {
                if (std::find(nodes.begin(), nodes.end(), *it) ==
                    nodes.end()) {
                    nodes.push_back(*it);
                }
                if (token->parents_.size() == 1 &&
                    token->parents_[0] == ROOTID) {
                    result.insert(token->fileId_);
                }
            }
        }
        return result;
    }

    /** get a file from index, or add it if files is not present in index*/
    uint32_t getFile(const string &filename, uint32_t parent = ROOTID) {
        SourceInfo *found = find(filename);
        if (found != nullptr) {
            return found->fileId_;
        } else {
            uint32_t ret = files_.size();
            push(ret, filename, 0, 1, &parent);
            return ret;
        }
    }

    /** set parent-child dependency*/
    inline void link(uint32_t file, uint32_t pid) {
        vector<uint32_t> &parents = files_[file].parents_;
        const auto &end = parents.end();
        if (std::find(parents.begin(), end, pid) == end) {
            // remove wrong TU's if they have another dependencies
            if (parents.size() == 1 &&
                parents[0] == ROOTID) {
                parents.resize(0);
            }
            parents.push_back(pid);
        }
    }

    inline const string& fileName(uint32_t fid) {
        if (fid < files_.size()) {
            return files_[fid].fileName();
        }
        static string error("<error>");
        return error;
    }

    inline const string& projectPath() {
        return files_[ROOTID].filename_;
    }

    inline void setGlobalArgs(const string &args) {
        files_[ROOTID].setArgs(args);
    }

    void fillIncludes(uint32_t file, unordered_set<string> *includes) const {
        const vector<string> &arguments = args(file);
        for (const auto &s : arguments) {
            if (s.length() > 2 && s[0] == '-' && s[1] == 'I') {
                includes->emplace(s.c_str() + 2, s.length() - 2);
            }
        }
    }

    virtual bool parse(uint32_t fid, bool recursive) = 0;
    virtual void preprocess(uint32_t fid) = 0;
    virtual void loadPCHData() = 0;
    virtual void completion(uint32_t fid, const string &prefix,
                            uint32_t line, uint32_t column) = 0;
    virtual ~CDEIndex() {
    }
};
