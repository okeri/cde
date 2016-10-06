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

#include "cdeindex.h"
#include "gccsupport.h"
#include "emacsmapper.h"

// TODO: replace time() with std::chrono::

enum PF_FLAGS {
    PF_NONE = 0x0,
    PF_ERRDIAG = 0x1,
    PF_ALLDIAG = 0x2,
    PF_ANYDIAG = 0x3,
    PF_BUILDMAP = 0x4,
    PF_NOTIMECHECK = 0x8
};

constexpr PF_FLAGS operator | (PF_FLAGS a, PF_FLAGS b) {
    return PF_FLAGS(unsigned(a) | unsigned(b));
}

using namespace clang;

class CDEIndexImpl : public CDEIndex,
                     public RecursiveASTVisitor<CDEIndexImpl> {
    friend class RecursiveASTVisitor<CDEIndexImpl>;

    ASTContext *context_;
    SourceManager *sm_;
    std::shared_ptr<PCHContainerOperations> pchOps_;
    std::map<uint32_t, ASTUnit*> units_;
    bool pch_;

    std::string getPCHFilename(uint32_t n);
    inline const FileEntry *feFromLocation(const SourceLocation &location) {
        FileID fileID = sm_->getFileID(location);

        bool invalid = false;
        const SrcMgr::SLocEntry &sloc =
                sm_->getSLocEntry(fileID, &invalid);
        if (invalid || !sloc.isFile()) {
            return nullptr;
        }
        return sm_->getFileEntryForSLocEntry(sloc);
    }

    std::string getLocStr(const SourceLocation &location,
                     uint32_t *pos, uint32_t *line = nullptr);

    uint32_t getLoc(const SourceLocation &location,
                    uint32_t *pos, uint32_t *line = nullptr);

    template <class D>
    inline void record(const SourceLocation &locRef, const D *decl,
                       bool fwd = false) {
        record(locRef, decl->getLocation(), fwd);
    }

    void handleDiagnostics(std::string tuFile, const StoredDiagnostic *begin,
                           const StoredDiagnostic *end,
                           bool onlyErrors);
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
    ASTUnit *parse(uint32_t tu, uint32_t file, PF_FLAGS flags);
    ASTUnit *getParsedTU(uint32_t fid, bool all, bool *parsed = nullptr);
    void getFirstError(const std::string &filename,
                       const StoredDiagnostic *begin,
                       const StoredDiagnostic *end, uint32_t *errline,
                       uint32_t *errcol);
  public:
    CDEIndexImpl(const std::string &projectPath, const std::string &storePath,
                 bool pch);
    bool parse(uint32_t fid, bool recursive);
    void completion(uint32_t fid, const std::string &prefix,
                    uint32_t line, uint32_t column);
    void preprocess(uint32_t fid);
    void loadPCHData();
    ~CDEIndexImpl();
};

class CiConsumer : public CodeCompleteConsumer {
    IntrusiveRefCntPtr<GlobalCodeCompletionAllocator> ccAllocator_;
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

    // TODO: cache completions  ???
    //    IntrusiveRefCntPtr<GlobalCodeCompletionAllocator> ccCachedAllocator;

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
                            std::cout << "  // "
                                      << completion->getBriefComment();
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

CDEIndexImpl::CDEIndexImpl(const std::string& projectPath,
                           const std::string& storePath, bool pch)
        : CDEIndex(projectPath, storePath),
          pchOps_(new PCHContainerOperations()), pch_(pch) {
}


CDEIndexImpl::~CDEIndexImpl() {
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


CDEIndex *createIndex(const std::string& projectPath,
                      const std::string& storePath, bool pch) {
    return new CDEIndexImpl(projectPath, storePath, pch);
}


bool CDEIndexImpl::VisitDeclRefExpr(DeclRefExpr *e) {
    record(e->getLocation(), e->getDecl());
    return true;
}

bool CDEIndexImpl::VisitCXXConstructExpr(CXXConstructExpr *e) {
    record(e->getLocation(), e->getConstructor());
    return true;
}

bool CDEIndexImpl::VisitMemberExpr(MemberExpr *e) {
    record(e->getMemberLoc(), e->getMemberDecl()->getCanonicalDecl());
    return true;
}

void CDEIndexImpl::record(const SourceLocation &locRef,
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


bool CDEIndexImpl::VisitTypeLoc(TypeLoc tl) {
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


bool CDEIndexImpl::VisitDecl(Decl *declaration) {
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



void CDEIndexImpl::preprocess(uint32_t fid) {
    bool parsed = false;
    ASTUnit * unit  = getParsedTU(fid, true, &parsed);
    if (!parsed) {
        preprocessTUforFile(unit, fid, false);
        handleDiagnostics(unit->getASTFileName(), unit->stored_diag_begin(),
                          unit->stored_diag_end(), false);
    }
}


ASTUnit *CDEIndexImpl::getParsedTU(uint32_t fid, bool all, bool *parsed) {
    uint32_t tu = getAnyTU(fid);
    const auto &unitIter = units_.find(tu);
    if (unitIter != units_.end()) {
        return unitIter->second;
    } else {
        if (parsed != nullptr) {
            *parsed = true;
        }
        return parse(tu, fid, PF_BUILDMAP | PF_NOTIMECHECK |
                     (all ? PF_ALLDIAG : PF_ERRDIAG));
    }
    return nullptr;
}


bool CDEIndexImpl::parse(uint32_t fid, bool recursive) {
    if (recursive) {
        std::unordered_set<uint32_t> tus = getAllTUs(fid);
        for (auto it : tus) {
            if (!parse(it, fid, PF_BUILDMAP | PF_ALLDIAG)) {
                return false;
            }
        }
        return true;
    } else {
        uint32_t tu = getAnyTU(fid);
        return parse(tu, fid, PF_ALLDIAG | PF_NOTIMECHECK) != nullptr;
    }
}

// looks like in most cases we need only buffer of current file
void CDEIndexImpl::completion(uint32_t fid,
                              const std::string &prefix, uint32_t line,
                              uint32_t column) {

    ASTUnit *unit = getParsedTU(fid, false);
    if (unit == nullptr) {
        return;
    }

    const std::string &filename = fileName(fid);
    // find error location and compare it to complete location
    if (unit->getDiagnostics().hasErrorOccurred() ||
        unit->getDiagnostics().hasFatalErrorOccurred()) {
        uint32_t errcol, errline;
        getFirstError(filename, unit->stored_diag_begin(),
                      unit->stored_diag_end(), &errline, &errcol);
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
                llvm::MemoryBuffer::getMemBufferCopy(file.second, file.first);
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


    if (consumer.diag->hasErrorOccurred() ||
        consumer.diag->hasFatalErrorOccurred()) {
        sm_ = consumer.sourceMgr.get();
        handleDiagnostics(filename, consumer.diagnostics.begin(),
                          consumer.diagnostics.end(), true);
    }

    // TODO: ???
    //    consumer.ccCachedAllocator = unit->getCachedCompletionAllocator();
}

static const char *getClangIncludeArg() {
    static std::string clangInc("-I");
    if (clangInc == "-I") {
        clangInc += LLVM_PREFIX;
        clangInc += "/lib/clang/";
        clangInc += CLANG_VERSION_STRING;
        clangInc += "/include";
    }
    return clangInc.c_str();
}

// no reference to filename here,
// because of relocations in preprocessTUforFile->getFile->push
void CDEIndexImpl::preprocessTUforFile(ASTUnit *unit, uint32_t fid,
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
    const std::vector<SourceRange> &skipped = pp->getSkippedRanges();
    std::vector<std::pair<uint32_t, uint32_t>> filtered;
    uint32_t b, e, dummy;
    std::string file;

    for (const auto &s : skipped) {
        file = getLocStr(s.getBegin(), &dummy, &b);
        if (file == filename && file == getLocStr(s.getEnd(), &dummy, &e)) {
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

ASTUnit *CDEIndexImpl::parse(uint32_t tu, uint32_t au, PF_FLAGS flags) {
    if (!(flags & PF_NOTIMECHECK) &&
        find(tu)->time() > fileutil::fileTime(fileName(tu))) {
        return nullptr;
    }

    std::unique_ptr<ASTUnit> errUnit;
    ASTUnit *unit;

    SmallVector<ASTUnit::RemappedFile, 4> remappedFiles;
    for (const auto &file : EmacsMapper::mapped()) {
        std::unique_ptr<llvm::MemoryBuffer> mb =
                llvm::MemoryBuffer::getMemBufferCopy(file.second, file.first);
        remappedFiles.emplace_back(file.first, mb.release());
    }

    const auto &unitIter = units_.find(tu);
    if (unitIter != units_.end()) {
        unit = unitIter->second;
        unit->Reparse(pchOps_, remappedFiles);
    } else {
        std::vector<const char *> args;
        args.reserve(16);

#if (CLANG_VERSION_MAJOR >= 3 && CLANG_VERSION_MINOR == 8)
        // TODO: why ?
        args.push_back("-Xclang");
#endif
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
#if (CLANG_VERSION_MAJOR >= 3 && CLANG_VERSION_MINOR > 8)
        diags->setFatalsAsError(true);
#endif
        unit = ASTUnit::LoadFromCommandLine(
            args.data(), args.data() + args.size(),
            pchOps_, diags, "", false, true, remappedFiles,
            false, 0,
            TU_Complete,
            true, true, true, false, true, false,

#if (CLANG_VERSION_MAJOR >= 3 && CLANG_VERSION_MINOR > 7)
            pchOps_->getRawReader().getFormat(),
#endif
            &errUnit);
        if (unit != nullptr) {
            units_[tu] = unit;
        } else {
            return nullptr;
        }
    }

    sm_ = &unit->getSourceManager();

    preprocessTUforFile(unit, au, flags & PF_BUILDMAP);

    if (flags & PF_ANYDIAG) {
        handleDiagnostics(fileName(tu), unit->stored_diag_begin(),
                          unit->stored_diag_end(),
                          (flags & PF_ERRDIAG));
    }

    // do not update records_ if errors are occured
    if (unit->getDiagnostics().hasErrorOccurred() ||
        unit->getDiagnostics().hasFatalErrorOccurred()) {
        return unit;
    }

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

static unsigned levelIndex(DiagnosticsEngine::Level level) {
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

void CDEIndexImpl::getFirstError(const std::string &filename,
                                 const StoredDiagnostic *begin,
                                 const StoredDiagnostic *end, uint32_t *errline,
                                 uint32_t *errcol) {
    for (auto it = begin; it != end; ++it) {
        if (levelIndex(it->getLevel()) == 3) {
            SourceLocation sl(it->getLocation());
            FileID fileID = sm_->getFileID(sl);
            bool invalid = false;
            SrcMgr::SLocEntry sloc = sm_->getSLocEntry(fileID, &invalid);
            if (invalid || !sloc.isFile()) {
                continue;
            }

            const FileEntry *fe = sm_->getFileEntryForSLocEntry(sloc);
            if (fe == nullptr) {
                continue;
            }
            std::string file = fe->getName();
            while (file != filename) {
                sl = sloc.getFile().getIncludeLoc();
                fileID = sm_->getFileID(sl);
                sloc = sm_->getSLocEntry(fileID, &invalid);
                if (invalid || !sloc.isFile()) {
                    break;
                }
                file = sm_->getFileEntryForSLocEntry(sloc)->getName();
            }
            if (!invalid && sloc.isFile()) {
                *errline = sm_->getExpansionLineNumber(sl);
                *errcol = sm_->getExpansionColumnNumber(sl);
                return;
            }
        }
    }
}

void CDEIndexImpl::handleDiagnostics(std::string tuFile,
                                     const StoredDiagnostic *begin,
                                     const StoredDiagnostic *end,
                                     bool onlyErrors) {
    if (begin == end) {
        std::cout << "(cde--error-rep)" << std::endl;
        return;
    }
    std::vector<std::string> errors;
    std::map<std::string, std::map<uint32_t, std::pair<unsigned, size_t>>>
            directs, links;

    for (const StoredDiagnostic* it = begin; it != end; ++it) {
        if (*it) {
            unsigned level = levelIndex(it->getLevel());
            if (!onlyErrors || level == 3) {
                SourceLocation sl(it->getLocation());
                FileID fileID = sm_->getFileID(sl);
                bool invalid = false;
                SrcMgr::SLocEntry sloc = sm_->getSLocEntry(fileID, &invalid);

                if (invalid || !sloc.isFile()) {
                    continue;
                }

                // register diagnostic message
                errors.push_back(it->getMessage().str());

                std::string file
                        = sm_->getFileEntryForSLocEntry(sloc)->getName();
                std::pair<unsigned, size_t> pos(level, errors.size() - 1);
                uint32_t line = sm_->getExpansionLineNumber(sl);

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
                while (file != tuFile) {
                    sl = sloc.getFile().getIncludeLoc();
                    fileID = sm_->getFileID(sl);
                    sloc = sm_->getSLocEntry(fileID, &invalid);
                    if (invalid || !sloc.isFile()) {
                        break;
                    }
                    file = sm_->getFileEntryForSLocEntry(sloc)->getName();
                    line = sm_->getExpansionLineNumber(sl);
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
        std::cout << "(cde--error-rep)" << std::endl;
        return;
    }

    std::cout << "(cde--error-rep [";
    // construct errors list
    for (const auto& it : errors) {
        std::cout << quoted(it) << " ";
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

std::string CDEIndexImpl::getLocStr(const SourceLocation &location,
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

uint32_t CDEIndexImpl::getLoc(const SourceLocation &location,
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

bool CDEIndexImpl::TraverseNestedNameSpecifierLoc(NestedNameSpecifierLoc nnsl) {
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

std::string CDEIndexImpl::getPCHFilename(uint32_t n) {
    return storePath_ + SEPARATOR + std::to_string(n) + ".pch";
}


void CDEIndexImpl::loadPCHData() {
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
#if (CLANG_VERSION_MAJOR >= 3 && CLANG_VERSION_MINOR > 7)
                                                    false,
#endif
                                                    None,
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
