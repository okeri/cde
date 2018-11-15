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

#include <llvm/Config/llvm-config.h>

#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/Basic/Diagnostic.h>
#include <clang/Basic/DiagnosticOptions.h>
#include <clang/Basic/Version.h>
#include <clang/Frontend/ASTUnit.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/CompilerInvocation.h>
#include <clang/Frontend/Utils.h>
#include <clang/Lex/PreprocessingRecord.h>
#include <clang/Lex/Preprocessor.h>
#include <clang/Sema/CodeCompleteConsumer.h>

#include <charconv>
#include <iostream>
#include <map>

#include "cdeindex.h"
#include "emacsmapper.h"
#include "gccsupport.h"

#if (CLANG_VERSION_MAJOR < 4)
#error "Unsupported version of clang"
#endif

using namespace clang;

namespace {

#if !defined(LLVM_PREFIX) && defined(CONFIGURED_LLVM_PREFIX)
#define LLVM_PREFIX CONFIGURED_LLVM_PREFIX
#endif

const char* getClangIncludeArg() {
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

void printQuoted(const char* str) {
    char *head = nullptr, *tail = const_cast<char*>(str);
    for (; *tail != '\0'; ++tail) {
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

size_t scoredAlike(std::string_view s1, const std::string& s2) {
    for (size_t i = 0; i < s1.length(); ++i)
        if (s2.length() < i || s1[i] != s2[i]) {
            return i;
        }
    return s1.length();
}

}  // namespace

// CiConsumer class
class CiConsumer final : public CodeCompleteConsumer {
    std::shared_ptr<GlobalCodeCompletionAllocator> ccAllocator_;
    CodeCompletionTUInfo info_;
    IntrusiveRefCntPtr<DiagnosticOptions> diagOpts_;
    std::string_view prefix_;

  public:
    IntrusiveRefCntPtr<DiagnosticsEngine> diag;
    IntrusiveRefCntPtr<FileManager> fileMgr;
    IntrusiveRefCntPtr<SourceManager> sourceMgr;
    LangOptions langOpts;
    SmallVector<StoredDiagnostic, 8> diagnostics;
    SmallVector<const llvm::MemoryBuffer*, 1> temporaryBuffers;

  public:
    CiConsumer(const CodeCompleteOptions& CodeCompleteOpts,
        IntrusiveRefCntPtr<FileManager> fm, std::string_view prefix) :
        CodeCompleteConsumer(CodeCompleteOpts, false),
        ccAllocator_(std::make_shared<GlobalCodeCompletionAllocator>()),
        info_(ccAllocator_),
        diagOpts_(new DiagnosticOptions),
        prefix_(prefix),
        diag(new DiagnosticsEngine(
            IntrusiveRefCntPtr<DiagnosticIDs>(new DiagnosticIDs), &*diagOpts_)),
        fileMgr(fm),
        sourceMgr(new SourceManager(*diag, *fileMgr)) {
    }

    CodeCompletionAllocator& getAllocator() override {
        return info_.getAllocator();
    }

    CodeCompletionTUInfo& getCodeCompletionTUInfo() override {
        return info_;
    }

    void ProcessCodeCompleteResults(Sema& sema, CodeCompletionContext context,
        CodeCompletionResult* results, unsigned numResults) override {
        if (!numResults || diag->hasErrorOccurred() ||
            diag->hasFatalErrorOccurred()) {
            std::cout << "(funcall cde--callback '())" << std::endl;
            return;
        }

        std::stable_sort(results, results + numResults);
        bool hasFilteredResults = false;
        // Print the results.
        for (unsigned i = 0; i != numResults; ++i) {
            CodeCompletionString* completions =
                results[i].CreateCodeCompletionString(sema, context,
                    info_.getAllocator(), info_, includeBriefComments());

            if (!completions->getAvailability()) {
                std::string_view entry = completions->getTypedText();
                if (entry != "" && entry.substr(0, 8) != "operator" &&
                    entry[0] != '~') {
                    if (prefix_ == "" ||
                        prefix_ == entry.substr(0, prefix_.length())) {
                        if (!hasFilteredResults) {
                            std::cout << "(cde--handle-completions '(";
                            hasFilteredResults = true;
                        }
                        std::cout << "(\"";
                        bool started = false;
                        size_t annoLen = 0, resultLen = 0;
                        for (const auto& completion : *completions) {
                            switch (completion.Kind) {
                                case CodeCompletionString::CK_Optional:
                                    break;
                                case CodeCompletionString::CK_VerticalSpace:
                                    std::cout << " ";
                                    break;
                                case CodeCompletionString::CK_Text:
                                case CodeCompletionString::CK_Informative:
                                    std::cout << completion.Text;
                                    if (resultLen != 0 && !started) {
                                        resultLen += strlen(completion.Text);
                                    }
                                    break;
                                case CodeCompletionString::CK_ResultType:
                                    std::cout << completion.Text << " ";
                                    resultLen += strlen(completion.Text) + 1;
                                    break;
                                case CodeCompletionString::CK_LeftParen:
                                    started = true;
                                    [[fallthrough]];

                                default:
                                    std::cout << completion.Text;
                                    break;
                            }

                            if (started) {
                                if (completion.Kind != CodeCompletionString::
                                                           CK_VerticalSpace &&
                                    completion.Kind !=
                                        CodeCompletionString::CK_Optional) {
                                    annoLen += strlen(completion.Text);
                                }
                            }
                        }

                        auto comment = completions->getBriefComment();
                        if (comment != nullptr) {
                            std::cout << "  // ";
                            printQuoted(comment);
                        }
                        std::cout << "\" " << resultLen << " ";
                        resultLen += entry.length();
                        std::cout << resultLen;
                        if (comment != nullptr) {
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

class CDEIndex::Impl final : public RecursiveASTVisitor<CDEIndex::Impl> {
    friend class RecursiveASTVisitor<CDEIndex::Impl>;

    // assume some average project has 512 < files < 1024.
    enum { MinParentNodeAlloc = 0x100, MinIndexAlloc = 0x400 };

    std::string storePath_;
    std::vector<SourceInfo> files_;
    std::map<size_t, size_t> hfilenames_;
    std::hash<std::string_view> hashStr;
    std::map<CI_KEY, CI_DATA> records_;
    SourceManager* sm_;
    std::shared_ptr<PCHContainerOperations> pchOps_;
    std::map<uint32_t, std::unique_ptr<ASTUnit>> units_;

  private:
    /** get translation unit for current file*/
    uint32_t getAnyTU(uint32_t file);

    /** get all translation units for current file*/
    const std::unordered_set<uint32_t> getAllTUs(uint32_t file);

    /** set parent-child dependency*/
    void link(uint32_t file, uint32_t pid);

    /** get SourceInfo or nullptr by filename */
    SourceInfo* find(std::string_view filename);

    /** get SourceInfo or nullptr by file id */
    SourceInfo* find(uint32_t fid);

    bool haveNostdinc(uint32_t file) const;
    void copyArgsToClangArgs(
        uint32_t file, std::vector<const char*>* clang_args) const;

    const FileEntry* feFromLocation(const SourceLocation& location);
    const SourceInfo* getDominatedParent(const SourceInfo* si) const;
    const std::vector<std::string>& args(uint32_t file) const;

    std::string getLocStr(const SourceLocation& location, uint32_t* pos,
        uint32_t* line = nullptr);

    template <class P, class D>
    SourceRange getParentSourceRangeOrSelf(D* node, int levels = 1) {
        auto parents = node->getASTContext().getParents(*node);
        if (!parents.empty()) {
            if (levels > 1) {
                // must be sure all direct parents except last one is Decl )
                auto next = parents.begin()->template get<Decl>();
                if (next) {
                    return getParentSourceRangeOrSelf<P>(next, levels - 1);
                }
            }
            auto parent = parents.begin()->template get<P>();
            if (parent) {
                return parent->getSourceRange();
            }
        }
        return node->getSourceRange();
    }

    SourceRange extend(const SourceRange& in, uint32_t offset) {
        return in.getEnd().isValid() ? SourceRange(in.getBegin(),
                                           in.getEnd().getLocWithOffset(offset))
                                     : in;
    }

    template <class D>
    void record(const SourceLocation& locRef, const D* decl, bool fwd = false) {
        bool skipMethod = false;
        switch (decl->getKind()) {
            case Decl::Function:
                skipMethod = true;
                [[fallthrough]];

            case Decl::CXXConstructor:
            case Decl::CXXDestructor:
            case Decl::CXXConversion:
            case Decl::CXXMethod:
                if (auto function = cast<FunctionDecl>(decl); function) {
                    if (function->doesThisDeclarationHaveABody()) {
                        record(locRef, decl->getLocation(),
                            SourceRange(function->getLocStart(),
                                function->getBody()->getLocStart()),
                            fwd);
                        return;
                    }
                    if (!skipMethod) {
                        if (auto method = cast<CXXMethodDecl>(function);
                            method) {
                            if (method->isConst()) {
                                record(locRef, decl->getLocation(),
                                    decl->getSourceRange(), fwd);
                                return;
                            }
                        }
                    }
                    record(locRef, decl->getLocation(),
                        extend(decl->getSourceRange(), 1), fwd);
                }
                break;

            case Decl::ParmVar:
                record(locRef, decl->getLocation(),
                    extend(getParentSourceRangeOrSelf<TypeLoc>(decl), 1));
                break;

            case Decl::EnumConstant:
                record(locRef, decl->getLocation(),
                    cast<EnumConstantDecl>(decl)->getSourceRange());
                break;

            case Decl::Binding:
                record(locRef, decl->getLocation(),
                    getParentSourceRangeOrSelf<DeclStmt>(decl, 2));
                break;

            case Decl::Var:
                record(locRef, decl->getLocation(),
                    getParentSourceRangeOrSelf<DeclStmt>(decl));
                break;

            case Decl::Field:
                if (auto next = decl->getNextDeclInContext(); next) {
                    record(locRef, decl->getLocation(),
                        SourceRange(decl->getLocStart(), next->getLocStart()),
                        false);
                }
                break;
                //            case Decl::EnumConstant:
            case Decl::CXXRecord:
            case Decl::Namespace:
            case Decl::ClassTemplate:
                record(locRef, decl->getLocation(),
                    SourceRange(decl->getLocStart(), SourceLocation()));
                break;

            default:
                record(locRef, decl->getLocation(),
                    extend(decl->getSourceRange(), 1));
                break;
        }
    }
    void handleDiagnostics(uint32_t marker, const std::string& tuFile,
        const StoredDiagnostic* begin, const StoredDiagnostic* end,
        bool onlyErrors, uint32_t* errline = nullptr,
        uint32_t* errcol = nullptr);
    void record(const SourceLocation& locRef, const SourceLocation& locDef,
        const SourceRange& locRangeDef, bool fwd = false);
    bool VisitDecl(Decl* d);
    bool VisitDeclRefExpr(DeclRefExpr* e);
    bool VisitCXXConstructExpr(CXXConstructExpr* e);
    bool VisitMemberExpr(MemberExpr* e);
    bool VisitTypeLoc(TypeLoc tl);
    bool TraverseNestedNameSpecifierLoc(NestedNameSpecifierLoc nnsl);

    void preprocessTUforFile(ASTUnit* unit, uint32_t fid, bool buildMap);
    ASTUnit* parse(uint32_t tu, bool cache = true);
    std::pair<uint32_t, ASTUnit*> getParsedTU(uint32_t fid);

  public:
    Impl(std::string_view projectPath, std::string_view storePath) noexcept;
    ~Impl();
    void set(CI_KEY* key, CI_DATA* data);
    const std::map<CI_KEY, CI_DATA>& records() const;
    const std::string& projectPath();
    const char* fileName(uint32_t fid);
    std::vector<SourceInfo>::const_iterator begin();
    std::vector<SourceInfo>::const_iterator end();
    void setGlobalArgs(std::string_view args);
    void setUnitWithArgs(
        std::string_view filename, std::vector<std::string>&& args);
    void push(uint32_t id, std::string_view path, uint32_t time,
        uint32_t parentCount, uint32_t* parents);
    std::vector<std::string> includes(
        uint32_t file, std::string_view relative) const;

    uint32_t findFile(std::string_view filename);

    uint32_t getFile(std::string_view filename, uint32_t parent = RootId);
    uint32_t getFile(const llvm::StringRef& filename, uint32_t parent = RootId);
    bool parse(uint32_t fid, ParseOptions options);
    void preprocess(uint32_t fid);
    void completion(uint32_t fid, std::string_view prefix, uint32_t line,
        std::uint32_t column);
};

template <>
void CDEIndex::Impl::record<MacroDefinitionRecord>(
    const SourceLocation& locRef, const MacroDefinitionRecord* decl, bool) {
    record(locRef, decl->getLocation(),
        SourceRange(decl->getSourceRange().getBegin(), SourceLocation()));
}

// CDEIndex::Impl implementation

CDEIndex::Impl::Impl(
    std::string_view projectPath, std::string_view storePath) noexcept :
    storePath_(storePath),
    pchOps_(new PCHContainerOperations()) {
    files_.reserve(MinIndexAlloc);
    push(0, projectPath, 0, 0, nullptr);
}

CDEIndex::Impl::~Impl() {
}

void CDEIndex::Impl::set(CI_KEY* key, CI_DATA* data) {
    records_[*key] = *data;
}

const std::map<CI_KEY, CI_DATA>& CDEIndex::Impl::records() const {
    return records_;
}

const std::string& CDEIndex::Impl::projectPath() {
    return files_[RootId].filename_;
}

const char* CDEIndex::Impl::fileName(uint32_t fid) {
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

void CDEIndex::Impl::setGlobalArgs(std::string_view args) {
    files_[RootId].setArgs(args);
}

void CDEIndex::Impl::link(uint32_t file, uint32_t pid) {
    std::vector<uint32_t>& parents = files_[file].parents_;
    const auto& end = parents.end();
    if (std::find(parents.begin(), end, pid) == end) {
        // remove wrong TU's if they have another dependencies
        if (parents.size() == 1 && parents[0] == RootId) {
            parents.resize(0);
        }
        parents.push_back(pid);
    }
}

void CDEIndex::Impl::setUnitWithArgs(
    std::string_view filename, std::vector<std::string>&& args) {
    find(getFile(filename))->args_ = std::move(args);
}

void CDEIndex::Impl::push(uint32_t id, std::string_view path, uint32_t time,
    uint32_t parentCount, uint32_t* parents) {
    const SourceInfo& info =
        files_.emplace_back(id, std::string(path), time, parentCount, parents);
    hfilenames_.emplace(hashStr(info.filename_), info.fileId_);
}

std::vector<std::string> CDEIndex::Impl::includes(
    uint32_t file, std::string_view relative) const {
    const std::vector<std::string>& arguments = args(file);
    std::vector<std::string> result;

    for (const auto& s : arguments) {
        if (s.length() > 2 && s[0] == '-' && s[1] == 'I') {
            result.emplace_back(s.c_str() + 2, s.length() - 2);
        }
    }

    if (!relative.empty()) {
        result.emplace_back(relative);
        std::stable_sort(result.begin(), result.end(),
            [&relative](const std::string& a, const std::string& b) {
                return scoredAlike(relative, a) > scoredAlike(relative, b);
            });
    }
    return result;
}

uint32_t CDEIndex::Impl::findFile(std::string_view filename) {
    const auto& end = files_.end();
    if (auto it = find_if(files_.begin(), end,
            [filename](const SourceInfo& si) {
                return fileutil::hasTail(si.filename_, filename);
            });
        it != end) {
        return it->fileId_;
    }
    return INVALID;
}

uint32_t CDEIndex::Impl::getFile(std::string_view filename, uint32_t parent) {
    if (auto found = find(filename); found != nullptr) {
        return found->fileId_;
    } else {
        uint32_t ret = files_.size();
        push(ret, filename, 0, 1, &parent);
        return ret;
    }
}

uint32_t CDEIndex::Impl::getFile(
    const llvm::StringRef& filename, uint32_t parent) {
    return getFile(std::string_view(std::string(filename)), parent);
}

const SourceInfo* CDEIndex::Impl::getDominatedParent(
    const SourceInfo* si) const {
    const SourceInfo* token = si;
    while (token->parents_.size() != 0 && token->args_.empty()) {
        token = &files_[token->parents_[0]];
    }
    return token;
}

const std::vector<std::string>& CDEIndex::Impl::args(uint32_t file) const {
    return getDominatedParent(&files_[file])->args_;
}

bool CDEIndex::Impl::haveNostdinc(uint32_t file) const {
    const std::vector<std::string>& arguments = args(file);
    return std::find_if(arguments.begin(), arguments.end(),
               [](const auto& arg) { return arg == "-nostdinc"; }) !=
           arguments.end();
}

void CDEIndex::Impl::copyArgsToClangArgs(
    uint32_t file, std::vector<const char*>* clang_args) const {
    const std::vector<std::string>& arguments = args(file);
    for (const auto& s : arguments) {
        clang_args->push_back(s.c_str());
    }
}

SourceInfo* CDEIndex::Impl::find(std::string_view filename) {
    auto it = hfilenames_.find(hashStr(filename));
    return it != hfilenames_.end() ? &files_[it->second] : nullptr;
}

SourceInfo* CDEIndex::Impl::find(uint32_t fid) {
    return fid < files_.size() ? &files_[fid] : nullptr;
}

bool CDEIndex::Impl::VisitDeclRefExpr(DeclRefExpr* e) {
    record(e->getLocation(), e->getDecl());
    return true;
}

bool CDEIndex::Impl::VisitCXXConstructExpr(CXXConstructExpr* e) {
    record(e->getLocation(), e->getConstructor());
    return true;
}

bool CDEIndex::Impl::VisitMemberExpr(MemberExpr* e) {
    record(e->getMemberLoc(), e->getMemberDecl()->getCanonicalDecl());
    return true;
}

void CDEIndex::Impl::record(const SourceLocation& locRef,
    const SourceLocation& locDef, const SourceRange& locRangeDef, bool fwd) {
    auto getLoc = [this](const SourceLocation& location) {
        SourceLocation expansionLoc(sm_->getExpansionLoc(location));
        if (auto fe = feFromLocation(expansionLoc); fe != nullptr) {
            return std::make_tuple(getFile(fe->getName()),
                sm_->getDecomposedLoc(expansionLoc).second,
                sm_->getExpansionLineNumber(expansionLoc));
        }
        return std::make_tuple(static_cast<uint32_t>(INVALID), 0u, 0u);
    };

    auto getFastLoc = [this](const SourceLocation& location) {
        SourceLocation expansionLoc(sm_->getExpansionLoc(location));
        return sm_->getDecomposedLoc(expansionLoc).second;
    };

    CI_KEY ref;
    CI_DATA def;
    uint32_t refline, line;
    if (locRef.isInvalid() || locDef.isInvalid()) {
        return;
    }
    std::tie(ref.file, ref.pos, refline) = getLoc(locRef);
    std::tie(def.file, def.pos, line) = getLoc(locDef);
    if (locRangeDef.getEnd().isValid()) {
        def.declBegin = getFastLoc(locRangeDef.getBegin());
        def.declEnd = getFastLoc(locRangeDef.getEnd());
    } else if (locRangeDef.getBegin().isValid()) {
        def.declBegin = getFastLoc(locRangeDef.getBegin());
    }

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
    if (auto tag = tl.getAs<TagTypeLoc>(); !tag.isNull()) {
        record(tl.getBeginLoc(), tag.getDecl());
        return true;
    }

    if (auto td = tl.getAs<TypedefTypeLoc>(); !td.isNull()) {
        record(tl.getBeginLoc(), td.getTypedefNameDecl());
        return true;
    }

    if (auto ttp = tl.getAs<TemplateTypeParmTypeLoc>(); !ttp.isNull()) {
        record(tl.getBeginLoc(), ttp.getDecl());
        return true;
    }

    if (auto ts = tl.getAs<TemplateSpecializationTypeLoc>(); !ts.isNull()) {
        if (auto decl = ts.getTypePtr()
                            ->getAs<TemplateSpecializationType>()
                            ->getAsCXXRecordDecl();
            decl) {
            record(tl.getBeginLoc(), decl);
        }
    }
    return true;
}

bool CDEIndex::Impl::VisitDecl(Decl* declaration) {
    const Decl* definition = nullptr;
    switch (declaration->getKind()) {
        case Decl::UsingDirective:
            definition =
                cast<UsingDirectiveDecl>(declaration)->getNominatedNamespace();
            break;

        case Decl::NamespaceAlias:
            definition = cast<NamespaceAliasDecl>(declaration)->getNamespace();
            break;
        case Decl::CXXConstructor:
        case Decl::CXXDestructor:
        case Decl::CXXConversion:
        case Decl::Function:
        case Decl::CXXMethod:
            if (auto function = cast<FunctionDecl>(declaration); function) {
                if (!function->isThisDeclarationADefinition()) {
                    const FunctionDecl* body = nullptr;
                    if (function->getBody(body)) {
                        record(body->getLocation(), declaration->getLocation(),
                            SourceRange(), true);
                    }
                }
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
    auto [tu, unit] = getParsedTU(fid);
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
        auto token = &files_[nodes[curNode]];
        for (auto it : token->parents_) {
            if (std::find(nodes.begin(), nodes.end(), it) == nodes.end()) {
                nodes.push_back(it);
            }
            if (token->parents_.size() == 1 && token->parents_[0] == RootId) {
                result.insert(token->fileId_);
            }
        }
    }
    return result;
}

[[nodiscard]] std::pair<uint32_t, ASTUnit*> CDEIndex::Impl::getParsedTU(
    uint32_t fid) {
    uint32_t tuId = getAnyTU(fid);
    if (const auto& unitIter = units_.find(tuId); unitIter != units_.end()) {
        return std::make_pair(unitIter->first, unitIter->second.get());
    }
    return std::make_pair(tuId, parse(tuId, true));
}

[[nodiscard]] bool CDEIndex::Impl::parse(uint32_t fid, ParseOptions options) {
    if (options != ParseOptions::Recursive) {
        uint32_t tu = getAnyTU(fid);

        uint32_t actual = 0;

        if (auto mapped = EmacsMapper::mapped().find(find(fid)->fileName());
            mapped != EmacsMapper::mapped().end()) {
            actual = mapped->second.second;
        }
        actual = std::max(actual, std::max(fileutil::fileTime(fileName(tu)),
                                      fileutil::fileTime(fileName(fid))));
        bool outdated = find(tu)->time() < actual;

        ASTUnit* unit(nullptr);
        if (const auto& unitIter = units_.find(tu); unitIter != units_.end()) {
            unit = unitIter->second.get();
        }

        bool result = true;
        if (outdated || (unit == nullptr && options == ParseOptions::Force)) {
            unit = parse(tu, options != ParseOptions::Forget);

            result = unit != nullptr;
            if (result && options == ParseOptions::Forget) {
                units_.erase(tu);
            }
        }
        if (options == ParseOptions::Force) {
            preprocessTUforFile(unit, fid, true);
            handleDiagnostics(tu, unit->getMainFileName(),
                unit->stored_diag_begin(), unit->stored_diag_end(), false);
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
void CDEIndex::Impl::completion(
    uint32_t fid, std::string_view prefix, uint32_t line, uint32_t column) {
    auto [tu, unit] = getParsedTU(fid);
    if (unit == nullptr) {
        return;
    }

    const std::string& filename = fileName(fid);
    // find error location and compare it to complete location
    if (unit->getDiagnostics().hasErrorOccurred() ||
        unit->getDiagnostics().hasFatalErrorOccurred()) {
        uint32_t errcol = column, errline = line;
        handleDiagnostics(tu, filename, unit->stored_diag_begin(),
            unit->stored_diag_end(), true, &errline, &errcol);

        if (errline < line || (errline == line && errcol < column)) {
            std::cout << "(funcall cde--callback '())" << std::endl;
            return;
        }
    }

    CodeCompleteOptions opts;
    opts.IncludeBriefComments = 1;
    opts.IncludeMacros = 1;
    CiConsumer consumer(opts, &unit->getFileManager(), prefix);

    SmallVector<ASTUnit::RemappedFile, 4> remappedFiles;
    for (const auto& [name, buffer] : EmacsMapper::mapped()) {
        std::unique_ptr<llvm::MemoryBuffer> mb =
            llvm::MemoryBuffer::getMemBufferCopy(buffer.first, name);
        remappedFiles.emplace_back(name, mb.release());
    }
    unit->CodeComplete(filename, line, column, remappedFiles,
        opts.IncludeMacros, opts.IncludeCodePatterns, opts.IncludeBriefComments,
        consumer, pchOps_, *consumer.diag, consumer.langOpts,
        *consumer.sourceMgr, *consumer.fileMgr, consumer.diagnostics,
        consumer.temporaryBuffers);
}

// no reference to filename here,
// because of relocations in preprocessTUforFile->getFile->push
void CDEIndex::Impl::preprocessTUforFile(
    ASTUnit* unit, uint32_t fid, bool buildMap) {
    auto pp = unit->getPreprocessor().getPreprocessingRecord();
    if (pp == nullptr) {
        std::cout << "(message \"warning: preprocessor inaccessible\")"
                  << std::endl;
        return;
    }

    for (const auto& it : *pp) {
        switch (it->getKind()) {
            case PreprocessedEntity::EntityKind::MacroExpansionKind:
                if (MacroExpansion * me(cast<MacroExpansion>(it)); me) {
                    if (auto* mdr = me->getDefinition(); mdr != nullptr) {
                        record(me->getSourceRange().getBegin(), mdr);
                    }
                }
                break;

            case PreprocessedEntity::EntityKind::InclusionDirectiveKind:
                if (buildMap) {
                    InclusionDirective* id(cast<InclusionDirective>(it));
                    auto fe = feFromLocation(id->getSourceRange().getBegin());
                    if (fe == nullptr) {
                        continue;
                    }

                    uint32_t parent = getFile(fe->getName());
                    if (auto ife = id->getFile(); ife != nullptr) {
                        link(getFile(ife->getName(), parent), parent);
                    }
                }
                break;

            default:
                break;
        }
    }

    const std::string& filename = fileName(fid);
    std::vector<std::pair<uint32_t, uint32_t>> filtered;
    uint32_t b, e, dummy;
    std::string file;

    for (const auto& s : pp->getSkippedRanges()) {
        if (file = getLocStr(s.getBegin(), &dummy, &b);
            file == filename && file == getLocStr(s.getEnd(), &dummy, &e)) {
            if (b != --e) {
                filtered.emplace_back(b, e);
            }
        }
    }

    if (!filtered.empty()) {
        std::cout << "(cde--hideif \"" << filename << "\" '(";
        for (const auto& [begin, end] : filtered) {
            std::cout << "(" << begin << " " << end << ")";
        }
        std::cout << "))" << std::endl;
    }
}

ASTUnit* CDEIndex::Impl::parse(uint32_t tu, bool cache) {
    std::unique_ptr<ASTUnit> errUnit;
    ASTUnit* unit;

    SmallVector<ASTUnit::RemappedFile, 4> remappedFiles;
    for (const auto& file : EmacsMapper::mapped()) {
        std::unique_ptr<llvm::MemoryBuffer> mb =
            llvm::MemoryBuffer::getMemBufferCopy(file.second.first, file.first);
        remappedFiles.emplace_back(file.first, mb.release());
    }

    if (const auto& unitIter = units_.find(tu); unitIter != units_.end()) {
        unit = unitIter->second.get();
        unit->Reparse(pchOps_, remappedFiles);
    } else {
        std::vector<const char*> args;
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
            const std::unordered_set<std::string>& gcc_includes =
                GccSupport::includes();
            for (const auto& i : gcc_includes) {
                args.push_back(i.c_str());
            }
        }

        args.push_back(fileName(tu));

        IntrusiveRefCntPtr<DiagnosticsEngine> diags(
            CompilerInstance::createDiagnostics(new DiagnosticOptions()));

        unit = ASTUnit::LoadFromCommandLine(args.data(),
            args.data() + args.size(), pchOps_, diags, "", false, true,
            remappedFiles, false, cache ? 1U : 0U, TU_Complete,
            false,  // Cache code completion
            true, true,
#if (CLANG_VERSION_MAJOR > 6)
            SkipFunctionBodiesScope::None,
#else
            false,
#endif

#if (CLANG_VERSION_MAJOR > 4)
            false,  // single file parse
#endif
            true, false, pchOps_->getRawReader().getFormat(), &errUnit);

        if (unit != nullptr) {
            units_[tu].reset(unit);
        } else {
            return nullptr;
        }
    }

    sm_ = &unit->getSourceManager();

    // clear records_
    const auto& end = records_.end();
    for (auto it = records_.begin(); it != end;) {
        if (it->first.file == tu || it->second.file == tu) {
            records_.erase(it++);
        } else {
            ++it;
        }
    }

    // traverse ast tree
    TraverseDecl(unit->getASTContext().getTranslationUnitDecl());

    find(tu)->setTime(time(NULL));
    return unit;
}

void CDEIndex::Impl::handleDiagnostics(uint32_t marker,
    const std::string& tuFile, const StoredDiagnostic* begin,
    const StoredDiagnostic* end, bool onlyErrors, uint32_t* errline,
    uint32_t* errcol) {
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
                if (fsl.isInvalid()) {
                    continue;
                }
                const SourceManager& sm = fsl.getManager();
                FileID fileID = fsl.getFileID();
                const FileEntry* fe = sm.getFileEntryForID(fileID);
                if (fe == nullptr) {
                    continue;
                }

                // register diagnostic message
                errors.push_back(it->getMessage().str());

                std::string file = fe->getName();
                auto pos = std::make_pair(level, errors.size() - 1);
                uint32_t line = fsl.getExpansionLineNumber();

                // add diagnostic to current file
                if (directs.find(file) == directs.end()) {
                    directs[file][line] = pos;
                } else {
                    const auto& found = directs[file].find(line);
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
                        const auto& found = links[file].find(line);
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
            std::cout << "(" << pit.first << ".(" << pit.second.first << " "
                      << pit.second.second << "))";
        }
        std::cout << "))";
    }
    std::cout << ") '(";
    // construct links position list
    for (const auto& it : links) {
        std::cout << "(\"" << it.first << "\".(";
        for (const auto& pit : it.second) {
            std::cout << "(" << pit.first << ".(" << pit.second.first << " "
                      << pit.second.second << "))";
        }
        std::cout << "))";
    }

    std::cout << "))" << std::endl;
}

std::string CDEIndex::Impl::getLocStr(
    const SourceLocation& location, uint32_t* pos, uint32_t* line) {
    SourceLocation expansionLoc(sm_->getExpansionLoc(location));
    if (auto fe = feFromLocation(expansionLoc); fe != nullptr) {
        *pos = sm_->getDecomposedLoc(expansionLoc).second;
        if (line) {
            *line = sm_->getExpansionLineNumber(expansionLoc);
        }
        return fe->getName();
    }
    return "<error>";
}

bool CDEIndex::Impl::TraverseNestedNameSpecifierLoc(
    NestedNameSpecifierLoc nnsl) {
    while (nnsl) {
        NestedNameSpecifier* nns = nnsl.getNestedNameSpecifier();
        switch (nns->getKind()) {
            case NestedNameSpecifier::Namespace:
                record(nnsl.getLocalBeginLoc(), nns->getAsNamespace());
                break;

            case NestedNameSpecifier::NamespaceAlias:
                record(nnsl.getLocalBeginLoc(), nns->getAsNamespaceAlias());
                break;

            case NestedNameSpecifier::TypeSpec:
            case NestedNameSpecifier::TypeSpecWithTemplate:
                if (auto tt = nns->getAsType()->getAs<TypedefType>()) {
                    record(nnsl.getLocalBeginLoc(), tt->getDecl());
                } else if (auto rt = nns->getAsType()->getAs<RecordType>()) {
                    record(nnsl.getLocalBeginLoc(), rt->getDecl());
                } else if (auto tst =
                               nns->getAsType()
                                   ->getAs<TemplateSpecializationType>()) {
                    if (auto decl =
                            tst->getTemplateName().getAsTemplateDecl()) {
                        if (auto templatedDecl = decl->getTemplatedDecl()) {
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

const FileEntry* CDEIndex::Impl::feFromLocation(
    const SourceLocation& location) {
    return sm_->getFileEntryForID(sm_->getFileID(location));
}

// CDEIndex implementation
CDEIndex::CDEIndex(
    std::string_view projectPath, std::string_view storePath) noexcept :
    pImpl_(std::make_unique<CDEIndex::Impl>(projectPath, storePath)) {
}

CDEIndex::~CDEIndex() {
}

void CDEIndex::set(CI_KEY* key, CI_DATA* data) {
    pImpl_->set(key, data);
}

const std::map<CI_KEY, CI_DATA>& CDEIndex::records() const {
    return pImpl_->records();
}

std::vector<SourceInfo>::const_iterator CDEIndex::begin() {
    return pImpl_->begin();
}

std::vector<SourceInfo>::const_iterator CDEIndex::end() {
    return pImpl_->end();
}

const char* CDEIndex::fileName(uint32_t fid) {
    return pImpl_->fileName(fid);
}

const std::string& CDEIndex::projectPath() {
    return pImpl_->projectPath();
}

void CDEIndex::setGlobalArgs(std::string_view args) {
    pImpl_->setGlobalArgs(args);
}

void CDEIndex::push(uint32_t id, std::string_view path, uint32_t time,
    uint32_t parentCount, uint32_t* parents) {
    pImpl_->push(id, path, time, parentCount, parents);
}

void CDEIndex::setUnitWithArgs(
    std::string_view filename, std::vector<std::string>&& args) {
    pImpl_->setUnitWithArgs(filename, std::move(args));
}

std::vector<std::string> CDEIndex::includes(
    uint32_t file, std::string_view relative) const {
    return pImpl_->includes(file, relative);
}

bool CDEIndex::parse(uint32_t fid, ParseOptions options) {
    return pImpl_->parse(fid, options);
}

void CDEIndex::preprocess(uint32_t fid) {
    pImpl_->preprocess(fid);
}

void CDEIndex::completion(uint32_t fid, std::string_view prefix, uint32_t line,
    std::uint32_t column) {
    pImpl_->completion(fid, prefix, line, column);
}

uint32_t CDEIndex::findFile(std::string_view filename) {
    return pImpl_->findFile(filename);
}

uint32_t CDEIndex::getFile(std::string_view filename, uint32_t parent) {
    return pImpl_->getFile(filename, parent);
}
