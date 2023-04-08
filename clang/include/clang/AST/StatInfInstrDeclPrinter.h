#include "clang/AST/ASTContext.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclObjC.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/DeclVisitor.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/PrettyPrinter.h"
#include "clang/Basic/Module.h"
#include "llvm/Support/raw_ostream.h"
#include "clang/Analysis/CallGraph.h"

#include <string>
#include <map>

namespace clang {
  class StatInfInstrDeclPrinter : public DeclVisitor<StatInfInstrDeclPrinter> {
    raw_ostream &Out;
    PrintingPolicy Policy;
    const ASTContext &Context;
    unsigned Indentation;
    bool PrintInstantiation;
    bool EnableStructuralAnalysis;
    bool EnableTemporalAnalysis;
    StringRef fullpath_instr_file;
    StringRef entrypoint_func_name;

    FunctionDecl *first_function_with_instrumentation = nullptr;

    raw_ostream& Indent() { return Indent(Indentation); }
    raw_ostream& Indent(unsigned Indentation);
    void ProcessDeclGroup(SmallVectorImpl<Decl*>& Decls);

    void Print(AccessSpecifier AS);
    void PrintConstructorInitializers(CXXConstructorDecl *CDecl,
                                      std::string &Proto);
    
    /// Print an Objective-C method type in parentheses.
    ///
    /// \param Quals The Objective-C declaration qualifiers.
    /// \param T The type to print.
    void PrintObjCMethodType(ASTContext &Ctx, Decl::ObjCDeclQualifier Quals,
                             QualType T);

    void PrintObjCTypeParams(ObjCTypeParamList *Params);

    bool function_start = false; // next CompoundStmt is a body function
    CallGraph *callgraph;

  public:
    StatInfInstrDeclPrinter(raw_ostream &Out, const PrintingPolicy &Policy,
                const ASTContext &Context, CallGraph *cg, 
                bool esa=true, bool eta=true, StringRef instr_file="", StringRef entrypoint="main",
                unsigned Indentation = 0, bool PrintInstantiation = false)
        : Out(Out), Policy(Policy), Context(Context), callgraph(cg), 
        EnableStructuralAnalysis(esa), EnableTemporalAnalysis(eta), fullpath_instr_file(instr_file),
        entrypoint_func_name(entrypoint), Indentation(Indentation),PrintInstantiation(PrintInstantiation) {}

    void VisitDeclContext(DeclContext *DC, bool Indent = true);

    void VisitTranslationUnitDecl(TranslationUnitDecl *D);
    void VisitTypedefDecl(TypedefDecl *D);
    void VisitTypeAliasDecl(TypeAliasDecl *D);
    void VisitEnumDecl(EnumDecl *D);
    void VisitRecordDecl(RecordDecl *D);
    void VisitEnumConstantDecl(EnumConstantDecl *D);
    void VisitEmptyDecl(EmptyDecl *D);
    void VisitFunctionDecl(FunctionDecl *D);
    void VisitFriendDecl(FriendDecl *D);
    void VisitFieldDecl(FieldDecl *D);
    void VisitVarDecl(VarDecl *D);
    void VisitLabelDecl(LabelDecl *D);
    void VisitParmVarDecl(ParmVarDecl *D);
    void VisitFileScopeAsmDecl(FileScopeAsmDecl *D);
    void VisitPragmaTIStmtDecl(PragmaTIStmtDecl *D);
    void VisitImportDecl(ImportDecl *D);
    void VisitStaticAssertDecl(StaticAssertDecl *D);
    void VisitNamespaceDecl(NamespaceDecl *D);
    void VisitUsingDirectiveDecl(UsingDirectiveDecl *D);
    void VisitNamespaceAliasDecl(NamespaceAliasDecl *D);
    void VisitCXXRecordDecl(CXXRecordDecl *D);
    void VisitLinkageSpecDecl(LinkageSpecDecl *D);
    void VisitTemplateDecl(const TemplateDecl *D);
    void VisitFunctionTemplateDecl(FunctionTemplateDecl *D);
    void VisitClassTemplateDecl(ClassTemplateDecl *D);
    void VisitClassTemplateSpecializationDecl(
                                            ClassTemplateSpecializationDecl *D);
    void VisitClassTemplatePartialSpecializationDecl(
                                     ClassTemplatePartialSpecializationDecl *D);
    void VisitObjCMethodDecl(ObjCMethodDecl *D);
    void VisitObjCImplementationDecl(ObjCImplementationDecl *D);
    void VisitObjCInterfaceDecl(ObjCInterfaceDecl *D);
    void VisitObjCProtocolDecl(ObjCProtocolDecl *D);
    void VisitObjCCategoryImplDecl(ObjCCategoryImplDecl *D);
    void VisitObjCCategoryDecl(ObjCCategoryDecl *D);
    void VisitObjCCompatibleAliasDecl(ObjCCompatibleAliasDecl *D);
    void VisitObjCPropertyDecl(ObjCPropertyDecl *D);
    void VisitObjCPropertyImplDecl(ObjCPropertyImplDecl *D);
    void VisitUnresolvedUsingTypenameDecl(UnresolvedUsingTypenameDecl *D);
    void VisitUnresolvedUsingValueDecl(UnresolvedUsingValueDecl *D);
    void VisitUsingDecl(UsingDecl *D);
    void VisitUsingEnumDecl(UsingEnumDecl *D);
    void VisitUsingShadowDecl(UsingShadowDecl *D);
    void VisitOMPThreadPrivateDecl(OMPThreadPrivateDecl *D);
    void VisitOMPAllocateDecl(OMPAllocateDecl *D);
    void VisitOMPRequiresDecl(OMPRequiresDecl *D);
    void VisitOMPDeclareReductionDecl(OMPDeclareReductionDecl *D);
    void VisitOMPDeclareMapperDecl(OMPDeclareMapperDecl *D);
    void VisitOMPCapturedExprDecl(OMPCapturedExprDecl *D);
    void VisitTemplateTypeParmDecl(const TemplateTypeParmDecl *TTP);
    void VisitNonTypeTemplateParmDecl(const NonTypeTemplateParmDecl *NTTP);

    void printTemplateParameters(const TemplateParameterList *Params,
                                 bool OmitTemplateKW = false);
    void printTemplateArguments(llvm::ArrayRef<TemplateArgument> Args,
                                const TemplateParameterList *Params);
    void printTemplateArguments(llvm::ArrayRef<TemplateArgumentLoc> Args,
                                const TemplateParameterList *Params);
    void prettyPrintAttributes(Decl *D);
    void prettyPrintPragmas(Decl *D);
    void printDeclType(QualType T, StringRef DeclName, bool Pack = false);

    void printGroup(Decl** Begin, unsigned NumDecls,
                      raw_ostream &Out, const PrintingPolicy &Policy,
                      unsigned Indentation);
  };
}