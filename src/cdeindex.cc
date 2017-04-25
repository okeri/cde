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

#include <llvm/Support/CrashRecoveryContext.h>
#include <llvm/Config/llvm-config.h>

#include <clang/Basic/Diagnostic.h>
#include <clang/Basic/DiagnosticOptions.h>
#include <clang/Basic/Version.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/CompilerInvocation.h>
#include <clang/Frontend/Utils.h>
#include <clang/Frontend/ASTUnit.h>
#include <clang/Sema/CodeCompleteConsumer.h>
#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/Lex/PreprocessingRecord.h>
#include <clang/Lex/Preprocessor.h>

#include <iostream>
#include <iomanip>
#include <map>

#include "cdeindex.h"
#include "gccsupport.h"
#include "emacsmapper.h"

#if (CLANG_VERSION_MAJOR < 4)
#error "Unsupported version of clang"
#endif

using namespace clang;

namespace {

const char *getClangIncludeArg() {
    static std::string clangInc("-I");
    if (clangInc == "-I") {
        clangInc += LLVM_PREFIX;
        clangInc += "/lib/clang/";
        clangInc += CLANG_VERSION_STRING;
        clangInc += "/include";
    }
    return clangInc.c_str();
}

unsigned levelIndex(DiagnosticsEngine::Level level) {
    switch (level) {
        case DiagnosticsEngine::Note:
        case DiagnosticsEngine::Remark:
            return 1;

        case DiagnosticsEngine::Warning:
            return 2;

        case DiagnosticsEngine::Error:
        case DiagnosticsEngine::Fatal:
            return 3;

        default:
            return 0;
    }
}

void printQuoted(const char *str) {
    char *head = nullptr, *tail = const_cast<char *>(str);
    for (;*tail != '\0'; ++tail) {
        if (*tail != '\\' && *tail != '\"') {
            if (head == nullptr) {
                head = tail;
            }
        } else {
            if (head != nullptr) {
                char hold = *tail;
                *tail = '\0';
                std::cout << head;
                *tail = hold;
            }
            std::cout << '\\' << *tail;
            head = nullptr;
        }
    }
    if (head != nullptr) {
        std::cout << head;
    }
}

}  // namespace


// CiConsumer class
class CiConsumer : public CodeCompleteConsumer {
    std::shared_ptr<GlobalCodeCompletionAllocator> ccAllocator_;
    CodeCompletionTUInfo info_;
    IntrusiveRefCntPtr<DiagnosticOptions> diagOpts_;
    const std::string &prefix_;

  public:
    IntrusiveRefCntPtr<DiagnosticsEngine> diag;
    IntrusiveRefCntPtr<FileManager> fileMgr;
    IntrusiveRefCntPtr<SourceManager> sourceMgr;
    LangOptions langOpts;
    SmallVector<StoredDiagnostic, 8> diagnostics;
    SmallVector<const llvm::MemoryBuffer *, 1> temporaryBuffers;

  public:
    CiConsumer(const CodeCompleteOptions &CodeCompleteOpts,
               IntrusiveRefCntPtr<FileManager> fm, const std::string &prefix)
            : CodeCompleteConsumer(CodeCompleteOpts, false),
              ccAllocator_(new GlobalCodeCompletionAllocator),
              info_(ccAllocator_),
              diagOpts_(new DiagnosticOptions),
              prefix_(prefix),
              diag(new DiagnosticsEngine(
                  IntrusiveRefCntPtr<DiagnosticIDs>(new DiagnosticIDs),
                  &*diagOpts_)),
              fileMgr(fm),
              sourceMgr(new SourceManager(*diag, *fileMgr)) {
    }

    CodeCompletionAllocator &getAllocator() override {
        return info_.getAllocator();
    }

    CodeCompletionTUInfo &getCodeCompletionTUInfo() override {
        return info_;
    }

    void ProcessCodeCompleteResults(Sema &sema,
                                    CodeCompletionContext context,
                                    CodeCompletionResult *results,
                                    unsigned numResults) override {
        if (!numResults || diag->hasErrorOccurred() ||
            diag->hasFatalErrorOccurred()) {
            std::cout << "(funcall cde--callback '())" << std::endl;
            return;
        }

        std::stable_sort(results, results + numResults);
        bool hasFilteredResults = false;
        // Print the results.
        for (unsigned i = 0; i != numResults; ++i) {
            CodeCompletionString *completion =
                    results[i].CreateCodeCompletionString(
                        sema, context, info_.getAllocator(), info_,
                        includeBriefComments());

            if (!completion->getAvailability()) {
                const std::string &entry = completion->getTypedText();
                if (entry != "" && strncmp("operator", entry.c_str(), 8) &&
                    entry[0] != '~') {
                    if (prefix_ == "" ||
                        !strncmp(prefix_.c_str(), entry.c_str(),
                                 prefix_.length())) {
                        if (!hasFilteredResults) {
                            std::cout << "(cde--handle-completions '(";
                            hasFilteredResults = true;
                        }
                        std::cout << "(\"";
                        bool started = false;
                        size_t annoLen = 0, resultLen = 0;
                        for (auto it = completion->begin();
                             it != completion->end(); ++it) {
                            switch (it->Kind) {
                                case CodeCompletionString::CK_Optional:
                                    break;
                                case CodeCompletionString::CK_VerticalSpace:
                                    std::cout << " ";
                                    break;
                                case CodeCompletionString::CK_Text:
                                case CodeCompletionString::CK_Informative:
                                    std::cout << it->Text;
                                    if (resultLen != 0 && !started) {
                                        resultLen += strlen(it->Text);
                                    }
                                    break;
                                case CodeCompletionString::CK_ResultType:
                                    std::cout << it->Text << " ";
                                    resultLen += strlen(it->Text) + 1;
                                    break;
                                case CodeCompletionString::CK_LeftParen:
                                    started = true;
                                default:
                                    std::cout << it->Text;
                                    break;
                            }

                            if (started) {
                                if (it->Kind !=
                                    CodeCompletionString::CK_VerticalSpace &&
                                    it->Kind !=
                                    CodeCompletionString::CK_Optional) {
                                    annoLen += strlen(it->Text);
                                }
                            }
                        }
                        if (completion->getBriefComment() != nullptr) {
                            std::cout << "  // ";
                            printQuoted(completion->getBriefComment());
                        }
                        std::cout << "\" " << resultLen << " ";
                        resultLen += entry.length();
                        std::cout << resultLen;
                        if (completion->getBriefComment() != nullptr) {
                            resultLen += annoLen;
                            std::cout << " " << resultLen;
                        }
                        std::cout << ")";
                    }
                }
            }
        }
        if (!hasFilteredResults) {
            std::cout << "(funcall cde--callback '())";
        } else {
            std::cout << "))" << std::endl;
        }
    }
};

// CDEIndex::Impl declaration

class CDEIndex::Impl : public RecursiveASTVisitor<CDEIndex::Impl> {
    friend class RecursiveASTVisitor<CDEIndex::Impl>; //FIXME

    // assume some average project has 512 < files < 1024.
    enum {
        MinParentNodeAlloc = 0x100,
        MinIndexAlloc = 0x400
    };

    std::string storePath_;
    bool pch_;

    std::vector<SourceInfo> files_;
    std::map<size_t, size_t> hfilenames_;
    std::hash<std::string> hashStr;

    std::unordered_map<CI_KEY, CI_DATA> records_;

    ASTContext *context_;
    SourceManager *sm_;
    std::shared_ptr<PCHContainerOperations> pchOps_;
    std::map<uint32_t, ASTUnit*> units_;

  private:
    /** get translation unit for current file*/
    uint32_t getAnyTU(uint32_t file);

    /** get all translation units for current file*/
    const std::unordered_set<uint32_t> getAllTUs(uint32_t file);

    /** set parent-child dependency*/
    void link(uint32_t file, uint32_t pid);

    /** get SourceInfo or nullptr by filename */
    SourceInfo* find(const std::string &filename);

    /** get SourceInfo or nullptr by file id */
    SourceInfo* find(uint32_t fid);

    bool haveNostdinc(uint32_t file) const;
    std::string getPCHFilename(uint32_t n);
    void copyArgsToClangArgs(uint32_t file,
                             std::vector<const char*> *clang_args) const;

    inline const FileEntry *feFromLocation(const SourceLocation &location);
    inline const SourceInfo* getDominatedParent(const SourceInfo * si) const;
    inline const std::vector<std::string> &args(uint32_t file) const;

    std::string getLocStr(const SourceLocation &location,
                          uint32_t *pos, uint32_t *line = nullptr);

    uint32_t getLoc(const SourceLocation &location,
                    uint32_t *pos, uint32_t *line = nullptr);

    template <class D>
    inline void record(const SourceLocation &locRef, const D *decl,
                       bool fwd = false) {
        record(locRef, decl->getLocation(), fwd);
    }
    void handleDiagnostics(uint32_t marker,
                           const std::string &tuFile,
                           const StoredDiagnostic *begin,
                           const StoredDiagnostic *end,
                           bool onlyErrors,
                           uint32_t *errline = nullptr,
                           uint32_t *errcol = nullptr);
    void record(const SourceLocation &locRef, const SourceLocation &locDef,
                bool fwd = false);
    bool VisitDecl(Decl *d);
    bool VisitDeclRefExpr(DeclRefExpr *e);
    bool VisitCXXConstructExpr(CXXConstructExpr *e);
    bool VisitMemberExpr(MemberExpr *e);
    bool VisitTypeLoc(TypeLoc tl);
    bool TraverseNestedNameSpecifierLoc(NestedNameSpecifierLoc nnsl);

    void preprocessTUforFile(ASTUnit *unit, uint32_t fid,
                             bool buildMap);
    ASTUnit *parse(uint32_t tu, uint32_t file, bool cache = true);
    ASTUnit *getParsedTU(uint32_t fid, uint32_t *tu);

  public:
    Impl(const std::string &projectPath, const std::string &storePath,
         bool pch);
    ~Impl();
    inline void set(CI_KEY *key, CI_DATA *data);
    inline const std::unordered_map<CI_KEY, CI_DATA> &records() const;
    inline const std::string& projectPath();
    inline const char *fileName(uint32_t fid);
    inline std::vector<SourceInfo>::const_iterator begin();
    inline std::vector<SourceInfo>::const_iterator end();
    inline void setGlobalArgs(const std::string &args);
    void setUnitWithArgs(const std::string &filename,
                         std::vector<std::string> &&args);
    void push(uint32_t id, const std::string &path, uint32_t time,
              uint32_t parentCount, uint32_t *parents);
    void fillIncludes(uint32_t file,
                      std::unordered_set<std::string> *includes) const;

    uint32_t findFile(const std::string &filename);

    uint32_t getFile(const std::string &filename, uint32_t parent = RootId);
    bool parse(uint32_t fid, ParseOptions options);
    void preprocess(uint32_t fid);
    void loadPCHData();
    void completion(uint32_t fid, const std::string &prefix,
                    uint32_t line, std::uint32_t column);
};

// CDEIndex::Impl implementation

CDEIndex::Impl::Impl(const std::string& projectPath,
                     const std::string& storePath, bool pch)
        : pchOps_(new PCHContainerOperations()), pch_(pch) {
    files_.reserve(MinIndexAlloc);
    push(0, projectPath, 0, 0, nullptr);
}

CDEIndex::Impl::~Impl() {
    if (pch_) {
        fileutil::mkdir(storePath_);
        for (auto &u : units_) {
            if (u.second->getTranslationUnitKind() == TU_Complete) {
                u.second->Save(getPCHFilename(u.first));
            }
        }
    }
    for (auto &u : units_) {
        delete u.second;
    }
}

void CDEIndex::Impl::set(CI_KEY *key, CI_DATA *data) {
    records_[*key] = *data;
}

const std::unordered_map<CI_KEY, CI_DATA> &CDEIndex::Impl::records() const {
    return records_;
}

const std::string& CDEIndex::Impl::projectPath() {
    return files_[RootId].filename_;
}

const char *CDEIndex::Impl::fileName(uint32_t fid) {
    if (fid < files_.size()) {
        return files_[fid].fileName().c_str();
    }
    return "<error>";
}

std::vector<SourceInfo>::const_iterator CDEIndex::Impl::begin() {
    return files_.begin();
}

std::vector<SourceInfo>::const_iterator CDEIndex::Impl::end() {
    return files_.end();
}

void CDEIndex::Impl::setGlobalArgs(const std::string &args) {
    files_[RootId].setArgs(args);
}

void CDEIndex::Impl::link(uint32_t file, uint32_t pid) {
    std::vector<uint32_t> &parents = files_[file].parents_;
    const auto &end = parents.end();
    if (std::find(parents.begin(), end, pid) == end) {
        // remove wrong TU's if they have another dependencies
        if (parents.size() == 1 &&
            parents[0] == RootId) {
            parents.resize(0);
        }
        parents.push_back(pid);
    }
}

void CDEIndex::Impl::setUnitWithArgs(const std::string &filename,
                                     std::vector<std::string> &&args) {
    find(getFile(filename))->args_ = std::move(args);
}

void CDEIndex::Impl::push(uint32_t id, const std::string &path, uint32_t time,
                          uint32_t parentCount, uint32_t *parents) {
    files_.emplace_back(id, path, time, parentCount, parents);
    SourceInfo *ret = &files_[files_.size() - 1];
    hfilenames_.insert(std::make_pair(hashStr(ret->filename_),
                                      ret->fileId_));
}

void CDEIndex::Impl::fillIncludes(uint32_t file,
                                  std::unordered_set<std::string> *includes) const {
    const std::vector<std::string> &arguments = args(file);
    for (const auto &s : arguments) {
        if (s.length() > 2 && s[0] == '-' && s[1] == 'I') {
            includes->emplace(s.c_str() + 2, s.length() - 2);
        }
    }
}

uint32_t CDEIndex::Impl::findFile(const std::string &filename) {
    const auto &end = files_.end();
    auto it = find_if(
        files_.begin(), end,
        [filename] (const SourceInfo &si) {
            return fileutil::endsWith(si.filename_, filename, SEPARATOR);
        });
    if (it != end) {
        return it->fileId_;
    }
    return INVALID;
}

uint32_t CDEIndex::Impl::getFile(const std::string &filename, uint32_t parent) {
    SourceInfo *found = find(filename);
    if (found != nullptr) {
        return found->fileId_;
    } else {
        uint32_t ret = files_.size();
        push(ret, filename, 0, 1, &parent);
        return ret;
    }

}

const SourceInfo* CDEIndex::Impl::getDominatedParent(const SourceInfo * si) const {
    const SourceInfo *token = si;
    while (token->parents_.size() != 0 && token->args_.empty()) {
        token = &files_[token->parents_[0]];
    }
    return token;
}

const std::vector<std::string> &CDEIndex::Impl::args(uint32_t file) const {
    return getDominatedParent(&files_[file])->args_;
}

bool CDEIndex::Impl::haveNostdinc(uint32_t file) const {
    const std::vector<std::string> &arguments = args(file);
    for (const auto &s : arguments) {
        if (s == "-nostdinc") {
            return true;
        }
    }
    return false;
}

void CDEIndex::Impl::copyArgsToClangArgs(uint32_t file,
                                         std::vector<const char*> *clang_args) const {
    const std::vector<std::string> &arguments = args(file);
    for (const auto &s : arguments) {
        clang_args->push_back(s.c_str());
    }
}

SourceInfo* CDEIndex::Impl::find(const std::string &filename) {
    auto it = hfilenames_.find(hashStr(filename));
    return it != hfilenames_.end() ? &files_[it->second] : nullptr;
}

SourceInfo* CDEIndex::Impl::find(uint32_t fid) {
    return fid < files_.size() ? &files_[fid] : nullptr;
}

bool CDEIndex::Impl::VisitDeclRefExpr(DeclRefExpr *e) {
    record(e->getLocation(), e->getDecl());
    return true;
}

bool CDEIndex::Impl::VisitCXXConstructExpr(CXXConstructExpr *e) {
    record(e->getLocation(), e->getConstructor());
    return true;
}

bool CDEIndex::Impl::VisitMemberExpr(MemberExpr *e) {
    record(e->getMemberLoc(), e->getMemberDecl()->getCanonicalDecl());
    return true;
}

void CDEIndex::Impl::record(const SourceLocation &locRef,
                            const SourceLocation &locDef, bool fwd) {
    CI_KEY ref;
    CI_DATA def;
    uint32_t refline, line;
    if (locRef.isInvalid() || locDef.isInvalid()) return;
    ref.file = getLoc(locRef, &ref.pos, &refline);
    def.file = getLoc(locDef, &def.pos, &line);
    if (ref != def && def.file != INVALID) {
        def.flags = CI_DATA::None;
        def.refline = refline;
        records_[ref] = def;
        if (fwd) {
            def.flags = CI_DATA::Forward;
            ref.swapWithData(&def, line);
            records_[ref] = def;
        }
    }
}

bool CDEIndex::Impl::VisitTypeLoc(TypeLoc tl) {
    const TagTypeLoc &tag = tl.getAs<TagTypeLoc>();
    if (!tag.isNull()) {
        record(tl.getBeginLoc(), tag.getDecl());
        return true;
    }

    const TypedefTypeLoc &td = tl.getAs<TypedefTypeLoc>();
    if (!td.isNull()) {
        record(tl.getBeginLoc(), td.getTypedefNameDecl());
        return true;
    }

    const TemplateTypeParmTypeLoc &ttp = tl.getAs<TemplateTypeParmTypeLoc>();
    if (!ttp.isNull()) {
        record(tl.getBeginLoc(), ttp.getDecl());
        return true;
    }

    const TemplateSpecializationTypeLoc &ts =
            tl.getAs<TemplateSpecializationTypeLoc>();
    if (!ts.isNull()) {
        CXXRecordDecl *decl = ts.getTypePtr()
                ->getAs<TemplateSpecializationType>()->getAsCXXRecordDecl();
        if (decl) {
            record(tl.getBeginLoc(), decl);
        }
    }
    return true;
}

bool CDEIndex::Impl::VisitDecl(Decl *declaration) {
    const Decl *definition = nullptr;
    const FunctionDecl *function_def = nullptr;
    Decl::Kind kind = declaration->getKind();
    switch (kind) {
        case Decl::UsingDirective:
            definition = cast<UsingDirectiveDecl>(declaration)
                    ->getNominatedNamespace();
            break;

        case Decl::NamespaceAlias:
            definition = cast<NamespaceAliasDecl>(declaration)
                    ->getNamespace();
            break;

        case Decl::Function:
        case Decl::CXXMethod:
        case Decl::CXXConstructor:
        case Decl::CXXDestructor:
        case Decl::CXXConversion:
            if (cast<FunctionDecl>(declaration)->getBody(function_def)) {
                record(function_def->getLocation(), declaration, true);
            }
            return true;

        default:
            return true;
    }
    if (definition) {
        record(declaration->getLocation(), definition);
    }
    return true;
}


void CDEIndex::Impl::preprocess(uint32_t fid) {
    uint32_t tu;
    ASTUnit * unit  = getParsedTU(fid, &tu);
    preprocessTUforFile(unit, fid, true);
    handleDiagnostics(tu, unit->getMainFileName(), unit->stored_diag_begin(),
                      unit->stored_diag_end(), false);
}

uint32_t CDEIndex::Impl::getAnyTU(uint32_t file) {
    uint32_t token = file;
    while (files_[token].parents_.size() > 0 &&
           files_[token].parents_[0] != RootId) {
        token = files_[token].parents_[0];
    }
    return token;
}

const std::unordered_set<uint32_t> CDEIndex::Impl::getAllTUs(uint32_t file) {
    std::unordered_set<uint32_t> result;
    std::vector<uint32_t> nodes;
    unsigned curNode;
    nodes.reserve(MinParentNodeAlloc);
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
                token->parents_[0] == RootId) {
                result.insert(token->fileId_);
            }
        }
    }
    return result;
}

ASTUnit *CDEIndex::Impl::getParsedTU(uint32_t fid, uint32_t *tu) {
    *tu = getAnyTU(fid);
    const auto &unitIter = units_.find(*tu);
    if (unitIter != units_.end()) {
        return unitIter->second;
    } else {
        return parse(*tu, fid);
    }
    return nullptr;
}

bool CDEIndex::Impl::parse(uint32_t fid, ParseOptions options) {
    if (options != ParseOptions::Recursive) {
        uint32_t tu = getAnyTU(fid);

        uint32_t actual = 0;
        const auto &mapped = EmacsMapper::mapped().find(find(fid)->fileName());
        if (mapped != EmacsMapper::mapped().end()) {
            actual = mapped->second.second;
        }
        actual = std::max(actual,
                          std::max(fileutil::fileTime(fileName(tu)),
                                   fileutil::fileTime(fileName(fid))));
        bool outdated = find(tu)->time() < actual;

        ASTUnit *unit(nullptr);
        const auto &unitIter = units_.find(tu);
        if (unitIter != units_.end()) {
            unit = unitIter->second;
        }

        bool result = true;
        if (outdated || (unit == nullptr && options == ParseOptions::Force)) {
            unit = parse(tu, fid, options != ParseOptions::Forget);

            result = unit != nullptr;
            if (result && options == ParseOptions::Forget) {
                delete units_[tu];
                units_.erase(tu);
            }
        }
        if (options == ParseOptions::Force) {
            preprocessTUforFile(unit, fid, true);
            handleDiagnostics(tu, unit->getMainFileName(), unit->stored_diag_begin(),
                              unit->stored_diag_end(), false);
        }
        return result;
    } else {
        std::unordered_set<uint32_t> tus = getAllTUs(fid);
        for (auto it : tus) {
            if (!parse(it, ParseOptions::Forget)) {
                return false;
            }
        }
        return true;
    }
}

// looks like in most cases we need only buffer of current file
void CDEIndex::Impl::completion(uint32_t fid,
                                const std::string &prefix, uint32_t line,
                                uint32_t column) {
    uint32_t tu;
    ASTUnit *unit = getParsedTU(fid, &tu);
    if (unit == nullptr) {
        return;
    }

    const std::string &filename = fileName(fid);
    // find error location and compare it to complete location
    if (unit->getDiagnostics().hasErrorOccurred() ||
        unit->getDiagnostics().hasFatalErrorOccurred()) {
        sm_ = &unit->getSourceManager();
        uint32_t errcol, errline;
        handleDiagnostics(tu, filename, unit->stored_diag_begin(),
                          unit->stored_diag_end(), true,
                          &errline, &errcol);

        if (errline < line || (errline == line && errcol < column)) {
            std::cout << "(funcall cde--callback '())" << std::endl;
            return;
        }
    }

    CodeCompleteOptions opts;
    opts.IncludeBriefComments = 1;
    opts.IncludeMacros = 1;
    opts.IncludeCodePatterns = 0;
    CiConsumer consumer(opts, &unit->getFileManager(), prefix);

    SmallVector<ASTUnit::RemappedFile, 4> remappedFiles;
    for (const auto &file : EmacsMapper::mapped()) {
        std::unique_ptr<llvm::MemoryBuffer> mb =
                llvm::MemoryBuffer::getMemBufferCopy(file.second.first,
                                                     file.first);
        remappedFiles.emplace_back(file.first, mb.release());
    }
    unit->CodeComplete(filename, line, column, remappedFiles,
                       opts.IncludeMacros, opts.IncludeCodePatterns,
                       opts.IncludeBriefComments,
                       consumer,
                       pchOps_,
                       *consumer.diag,
                       consumer.langOpts,
                       *consumer.sourceMgr,
                       *consumer.fileMgr,
                       consumer.diagnostics,
                       consumer.temporaryBuffers);
}

// no reference to filename here,
// because of relocations in preprocessTUforFile->getFile->push
void CDEIndex::Impl::preprocessTUforFile(ASTUnit *unit, uint32_t fid,
                                         bool buildMap) {
    PreprocessingRecord *pp = unit->getPreprocessor()
                      .getPreprocessingRecord();

    if (pp == nullptr) {
        std::cout << "(message \"warning: preprocessor inaccessible\")"
                  << std::endl;
        return;
    }

    for (const auto &it : *pp) {
        switch (it->getKind()) {
            case PreprocessedEntity::EntityKind::MacroExpansionKind: {
                MacroExpansion *me(cast<MacroExpansion>(it));
                MacroDefinitionRecord *mdr = me->getDefinition();
                if (mdr != nullptr) {
                    record(me->getSourceRange().getBegin(), mdr);
                }
            }
                break;

            case PreprocessedEntity::EntityKind::InclusionDirectiveKind:
                if (buildMap) {
                    InclusionDirective *id(cast<InclusionDirective>(it));
                    const FileEntry *fe = feFromLocation(
                        id->getSourceRange().getBegin());

                    if (fe == nullptr) {
                        continue;
                    }

                    const FileEntry *ife = id->getFile();
                    uint32_t parent = getFile(fe->getName());
                    if (ife != nullptr) {
                        link(getFile(ife->getName(), parent), parent);
                    }
                }
                break;

            default:
                break;
        }
    }

    const std::string &filename = fileName(fid);
    std::vector<std::pair<uint32_t, uint32_t>> filtered;
    uint32_t b, e, dummy;
    std::string file;

    for (const auto &s : pp->getSkippedRanges()) {
        file = getLocStr(s.getBegin(), &dummy, &b);
        if (file == filename &&
            file == getLocStr(s.getEnd(), &dummy, &e)) {
            e--;
            if (b != e) {
                filtered.push_back(std::make_pair(b, e));
            }
        }
    }

    if (!filtered.empty()) {
        std::cout << "(cde--hideif \"" << filename << "\" '(";
        for (const auto &f : filtered) {
            std::cout << "(" << f.first << " " << f.second << ")";
        }
        std::cout << "))" << std::endl;
    }
}

ASTUnit *CDEIndex::Impl::parse(uint32_t tu, uint32_t au, bool cache) {
    std::unique_ptr<ASTUnit> errUnit;
    ASTUnit *unit;

    SmallVector<ASTUnit::RemappedFile, 4> remappedFiles;
    for (const auto &file : EmacsMapper::mapped()) {
        std::unique_ptr<llvm::MemoryBuffer> mb =
                llvm::MemoryBuffer::getMemBufferCopy(file.second.first,
                                                     file.first);
        remappedFiles.emplace_back(file.first, mb.release());
    }

    const auto &unitIter = units_.find(tu);
    if (unitIter != units_.end()) {
        unit = unitIter->second;
        unit->Reparse(pchOps_, remappedFiles);
    } else {
        std::vector<const char *> args;
        args.reserve(16);

        args.push_back("-fsyntax-only");
        args.push_back("-Xclang");
        args.push_back("-detailed-preprocessing-record");

        copyArgsToClangArgs(tu, &args);

        // We are not sure about language, so appending gcc c++ system include
        // paths to the end seems ok.
        if (!haveNostdinc(tu)) {
            // clang include path
            args.push_back(getClangIncludeArg());

            // gcc includes
            const std::unordered_set<std::string> &gcc_includes
                    = GccSupport::includes();
            for (const auto &i : gcc_includes) {
                args.push_back(i.c_str());
            }
        }

        args.push_back(fileName(tu));

        IntrusiveRefCntPtr<DiagnosticsEngine>
                diags(CompilerInstance::createDiagnostics(
                    new DiagnosticOptions()));
        diags->setFatalsAsError(true);

        unit = ASTUnit::LoadFromCommandLine(
            args.data(), args.data() + args.size(),
            pchOps_, diags, "", false, true, remappedFiles,
            false, cache ? 1 : 0, TU_Complete,
            true, true, true, false, true, false,
            pchOps_->getRawReader().getFormat(),
            &errUnit);

        if (unit != nullptr) {
            units_[tu] = unit;
        } else {
            return nullptr;
        }
    }

    sm_ = &unit->getSourceManager();

    // clear records_
    const auto &end = records_.end();
    for (auto it = records_.begin(); it != end;) {
        if (it->first.file == tu ||
            it->second.file == tu) {
            records_.erase(it++);
        } else {
            ++it;
        }
    }

    // traverse ast tree
    context_ = &unit->getASTContext();
    TraverseDecl(context_->getTranslationUnitDecl());

    find(tu)->setTime(time(NULL));
    return unit;
}

void CDEIndex::Impl::handleDiagnostics(uint32_t marker,
                                       const std::string &tuFile,
                                       const StoredDiagnostic *begin,
                                       const StoredDiagnostic *end,
                                       bool onlyErrors,
                                       uint32_t *errline,
                                       uint32_t *errcol) {
    if (begin == end) {
        std::cout << "(cde--error-rep " << marker << ")" << std::endl;
        return;
    }
    std::vector<std::string> errors;
    std::map<std::string, std::map<uint32_t, std::pair<unsigned, size_t>>>
            directs, links;

    for (const StoredDiagnostic* it = begin; it != end; ++it) {
        if (*it) {
            unsigned level = levelIndex(it->getLevel());
            if (!onlyErrors || level == 3) {
                FullSourceLoc fsl(it->getLocation());
                const SourceManager &sm = fsl.getManager();
                FileID fileID = fsl.getFileID();
                const FileEntry *fe = sm.getFileEntryForID(fileID);
                if (fe == nullptr) {
                    continue;
                }

                // register diagnostic message
                errors.push_back(it->getMessage().str());

                std::string file = fe->getName();
                std::pair<unsigned, size_t> pos(level, errors.size() - 1);
                uint32_t line = fsl.getExpansionLineNumber();

                // add diagnostic to current file
                if (directs.find(file) == directs.end()) {
                    directs[file][line] = pos;
                } else {
                    const auto &found = directs[file].find(line);
                    if (found == directs[file].end() ||
                        pos.first > found->first) {
                        directs[file][line] = pos;
                    }
                }

                // add includes chain
                SourceLocation sl = fsl;
                while (file != tuFile && sl.isValid()) {
                    sl = sm.getIncludeLoc(fileID);
                    if (!sl.isValid()) {
                        break;
                    }
                    fileID = sm.getFileID(sl);
                    line = sm.getExpansionLineNumber(sl);
                    fe = sm.getFileEntryForID(fileID);
                    if (fe == nullptr) {
                        continue;
                    }
                    file = fe->getName();

                    if (onlyErrors) {
                        onlyErrors = false;
                        *errline = line;
                        *errcol = sm.getExpansionColumnNumber(sl);
                    }

                    if (links.find(file) == links.end()) {
                        links[file][line] = pos;
                    } else {
                        const auto &found = links[file].find(line);
                        if (found == links[file].end() ||
                            pos.first > found->first) {
                            links[file][line] = pos;
                        }
                    }
                }
            }
        }
    }

    if (errors.empty()) {
        std::cout << "(cde--error-rep " << marker << ")" << std::endl;
        return;
    }

    std::cout << "(cde--error-rep " << marker << " [";
    // construct errors list
    for (const auto& it : errors) {
        std::cout << '"';
        printQuoted(it.c_str());
        std::cout << '"';
    }
    std::cout << "] '(";
    // construct direct position list
    for (const auto& it : directs) {
        std::cout << "(\"" << it.first << "\".(";
        for (const auto& pit : it.second) {
            std::cout << "(" << pit.first << ".(" << pit.second.first
                      << " " << pit.second.second << "))";
        }
        std::cout << "))";
    }
    std::cout << ") '(";
    // construct links position list
    for (const auto& it : links) {
        std::cout << "(\"" << it.first << "\".(";
        for (const auto& pit : it.second) {
            std::cout << "(" << pit.first << ".(" << pit.second.first
                      << " " << pit.second.second << "))";
        }
        std::cout << "))";
    }

    std::cout << "))" << std::endl;
}

std::string CDEIndex::Impl::getLocStr(const SourceLocation &location,
                                      uint32_t *pos, uint32_t *line) {
    SourceLocation expansionLoc(sm_->getExpansionLoc(location));
    const FileEntry *fe = feFromLocation(expansionLoc);
    if (fe != nullptr) {
        *pos = sm_->getDecomposedLoc(expansionLoc).second;
        if (line) {
            *line = sm_->getExpansionLineNumber(expansionLoc);
        }
        return fe->getName();
    }
    return "<error>";
}

uint32_t CDEIndex::Impl::getLoc(const SourceLocation &location,
                                uint32_t *pos, uint32_t *line) {
    SourceLocation expansionLoc(sm_->getExpansionLoc(location));
    const FileEntry *fe = feFromLocation(expansionLoc);
    if (fe != nullptr) {
        *pos = sm_->getDecomposedLoc(expansionLoc).second;
        if (line) {
            *line = sm_->getExpansionLineNumber(expansionLoc);
        }
        return getFile(fe->getName());
    }
    return INVALID;
}

bool CDEIndex::Impl::TraverseNestedNameSpecifierLoc(NestedNameSpecifierLoc nnsl) {
    while (nnsl) {
        NestedNameSpecifier *nns = nnsl.getNestedNameSpecifier();
        switch (nns->getKind()) {
            case NestedNameSpecifier::Namespace:
                record(nnsl.getLocalBeginLoc(), nns->getAsNamespace());
                break;

            case NestedNameSpecifier::NamespaceAlias:
                record(nnsl.getLocalBeginLoc(), nns->getAsNamespaceAlias());
                break;

            case NestedNameSpecifier::TypeSpec:
            case NestedNameSpecifier::TypeSpecWithTemplate:
                if (const TypedefType *tt = nns->getAsType()
                    ->getAs<TypedefType>()) {
                    record(nnsl.getLocalBeginLoc(), tt->getDecl());
                } else if (const RecordType *rt = nns->getAsType()
                           ->getAs<RecordType>()) {
                    record(nnsl.getLocalBeginLoc(), rt->getDecl());
                } else if (const TemplateSpecializationType *tst =
                           nns->getAsType()
                           ->getAs<TemplateSpecializationType>()) {
                    if (TemplateDecl *decl = tst->getTemplateName()
                        .getAsTemplateDecl()) {
                        if (NamedDecl *templatedDecl
                            = decl->getTemplatedDecl()) {
                            record(nnsl.getLocalBeginLoc(), templatedDecl);
                        }
                    }
                }
                break;

            default:
                break;
        }
        nnsl = nnsl.getPrefix();
    }
    return true;
}

const FileEntry *CDEIndex::Impl::feFromLocation(const SourceLocation &location) {
    FileID fileID = sm_->getFileID(location);

    bool invalid = false;
    const SrcMgr::SLocEntry &sloc =
            sm_->getSLocEntry(fileID, &invalid);
    if (invalid || !sloc.isFile()) {
        return nullptr;
    }
    return sm_->getFileEntryForSLocEntry(sloc);
}


std::string CDEIndex::Impl::getPCHFilename(uint32_t n) {
    return storePath_ + SEPARATOR + std::to_string(n) + ".pch";
}


void CDEIndex::Impl::loadPCHData() {
    // FIXME: disabled until llvm 3.9 will be released.
    // actually i dont know why is it crashes. But this is not
    // first-priority issue

    return;

    std::forward_list<std::string> files;
    fileutil::collectFiles(storePath_, &files, false);

    ASTUnit *unit;
    FileSystemOptions fsopts;
    for (const auto& it : files) {
        uint32_t id = 0;
        id = stoi(fileutil::basenameNoExt(it));
        if (id != 0) {
            const SourceInfo *si = find(id);
            if (fileutil::fileTime(si->fileName()) < si->time()) {
                std::cout << "(message \"tryreal " << si->fileName()
                          << "\")" << std::endl;
                auto readASTData = [=, &unit] {
                    IntrusiveRefCntPtr<DiagnosticsEngine>
                    diags(CompilerInstance::createDiagnostics(
                        new DiagnosticOptions()));
                    unit = ASTUnit::LoadFromASTFile(it, pchOps_->getRawReader(),
                                                    diags, fsopts, false,
                                                    false, None,
                                                    true).release();
                };

                llvm::CrashRecoveryContext CRC;
                if (CRC.RunSafelyOnThread(readASTData, 8 << 20)) {
                    if (unit) {
                        units_[id] = unit;
                        std::cout << "(message \"reloaded " << id
                                  << "\")" << std::endl;
                    }
                }
            }
        }
    }
}

// CDEIndex implementation
CDEIndex::CDEIndex(const std::string &projectPath, const std::string& storePath,
                   bool pch)
        : pImpl_(std::make_unique<CDEIndex::Impl>(projectPath, storePath, pch)) {
}

CDEIndex::~CDEIndex() {
}

void CDEIndex::set(CI_KEY *key, CI_DATA *data) {
    pImpl_->set(key, data);
}

const std::unordered_map<CI_KEY, CI_DATA> &CDEIndex::records() const {
    return pImpl_->records();
}

std::vector<SourceInfo>::const_iterator CDEIndex::begin() {
    return pImpl_->begin();
}

std::vector<SourceInfo>::const_iterator CDEIndex::end() {
    return pImpl_->end();
}

const char *CDEIndex::fileName(uint32_t fid) {
    return pImpl_->fileName(fid);
}

const std::string& CDEIndex::projectPath() {
    return pImpl_->projectPath();
}

void CDEIndex::setGlobalArgs(const std::string &args) {
    pImpl_->setGlobalArgs(args);
}

void CDEIndex::push(uint32_t id, const std::string &path, uint32_t time,
                    uint32_t parentCount, uint32_t *parents) {
    pImpl_->push(id, path, time, parentCount, parents);
}

void CDEIndex::setUnitWithArgs(const std::string &filename,
                               std::vector<std::string> &&args) {
    pImpl_->setUnitWithArgs(filename, std::move(args));
}

void CDEIndex::fillIncludes(uint32_t file,
                            std::unordered_set<std::string> *includes) const {
    pImpl_->fillIncludes(file, includes);
}

bool CDEIndex::parse(uint32_t fid, ParseOptions options) {
    return pImpl_->parse(fid, options);
}

void CDEIndex::preprocess(uint32_t fid) {
    pImpl_->preprocess(fid);
}

void CDEIndex::loadPCHData() {
    pImpl_->loadPCHData();
}

void CDEIndex::completion(uint32_t fid, const std::string &prefix,
                          uint32_t line, std::uint32_t column) {
    pImpl_->completion(fid, prefix, line, column);
}

uint32_t CDEIndex::findFile(const std::string &filename) {
    return pImpl_->findFile(filename);
}

uint32_t CDEIndex::getFile(const std::string &filename, uint32_t parent) {
    return pImpl_->getFile(filename, parent);
}