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

#include <cstring>
#include <cstdint>
#include <string>
#include <vector>
#include <memory>
#include <unordered_map>
#include <unordered_set>
#include <algorithm>
#include "fileutil.h"
#include "strbreak.h"

struct CI_DATA {
    enum Flags : uint8_t {
        None = 0x0,
        Forward = 0x1
    };
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


namespace std {

template <>
struct hash<CI_KEY> {
    size_t operator()(const CI_KEY& k) const {
        return hash<int>()(k.file) ^ (hash<int>()(k.pos) << 1);
    }
};

}  // namespace std


class SourceInfo {
    uint32_t fileId_;
    uint32_t updated_time_;
    std::string filename_;
    std::vector<std::string> args_;
    std::vector<uint32_t> parents_;

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

    SourceInfo(uint32_t fid, const std::string& filename,
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

    inline const std::string& fileName() const {
        return filename_;
    }

    inline void setArgs(const std::string& args) {
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
    class Impl;
    std::unique_ptr<Impl> pImpl_;

  public:
    enum class ParseOptions {
        Normal,
        Forget,
        Force,
        Recursive
    };

    enum { RootId = 0 };

  public:
    CDEIndex(const std::string &projectPath, const std::string& storePath,
             bool pch);
    ~CDEIndex();
    void set(CI_KEY *key, CI_DATA *data);
    const std::unordered_map<CI_KEY, CI_DATA> &records() const;
    const char *fileName(uint32_t fid);
    const std::string& projectPath();
    void setGlobalArgs(const std::string &args);
    std::vector<SourceInfo>::const_iterator begin();
    std::vector<SourceInfo>::const_iterator end();

    void push(uint32_t id, const std::string &path,
              uint32_t time = 0, uint32_t parentCount = 0,
              uint32_t *parents = nullptr);

    void setUnitWithArgs(const std::string &filename,
                         std::vector<std::string> &&args);

    std::vector<std::string> includes(uint32_t file,
                                      const std::string &relative = "") const;

    bool parse(uint32_t fid, ParseOptions options);
    void preprocess(uint32_t fid);
    void loadPCHData();
    void completion(uint32_t fid, const std::string &prefix,
                    uint32_t line, std::uint32_t column);

    /** find files in index  ending with filename*/
    uint32_t findFile(const std::string &filename);

    /** get a file from index, or add it if files is not present in index*/
    uint32_t getFile(const std::string &filename, uint32_t parent = RootId);
};
