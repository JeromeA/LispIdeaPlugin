package org.ax1.lisp.analysis;

import com.intellij.lang.ASTNode;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiManager;
import com.intellij.psi.search.FileTypeIndex;
import com.intellij.psi.search.GlobalSearchScope;
import org.ax1.lisp.LispFileType;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.analysis.symbol.SymbolBinding;
import org.ax1.lisp.analysis.symbol.SymbolManager;
import org.ax1.lisp.psi.*;
import org.jetbrains.annotations.NotNull;

import java.util.*;

import static com.intellij.lang.annotation.HighlightSeverity.INFORMATION;
import static com.intellij.openapi.editor.DefaultLanguageHighlighterColors.CONSTANT;
import static com.intellij.openapi.editor.colors.CodeInsightColors.NOT_USED_ELEMENT_ATTRIBUTES;
import static com.intellij.openapi.editor.colors.CodeInsightColors.WRONG_REFERENCES_ATTRIBUTES;
import static org.ax1.lisp.analysis.symbol.SymbolBinding.SymbolType.FUNCTION;
import static org.ax1.lisp.parsing.LispSyntaxHighlighter.KEYWORD;
import static org.ax1.lisp.psi.LispTypes.STRING;

public class SyntaxAnalyzer {

  private static final AnalyzeDefpackage ANALYZE_DEFPACKAGE = new AnalyzeDefpackage();
  private static final AnalyzeFunctionCall ANALYZE_FUNCTION_CALL = new AnalyzeFunctionCall();

  private static final Map<Symbol, Analyzer> analyzers = new HashMap<>();

  private final Project project;
  private final LispFile lispFile;
  private final AnnotationHolder holder;
  final SymbolManager symbolManager;
  final LexicalBindingManager lexicalBindings;

  public SyntaxAnalyzer(LispFile lispFile, @NotNull AnnotationHolder holder) {
    this.lispFile = lispFile;
    this.holder = holder;
    project = lispFile.getProject();
    symbolManager = SymbolManager.getInstance(project);
    lexicalBindings = new LexicalBindingManager(this);
    analyzers.put(getClSymbol("COND"), new AnalyzeCond());
    analyzers.put(getClSymbol("DEFMACRO"), new AnalyzeDefun(AnalyzeDefun.Type.DEFMACRO));
    analyzers.put(getClSymbol("DEFUN"), new AnalyzeDefun(AnalyzeDefun.Type.DEFUN));
    analyzers.put(getClSymbol("DEFVAR"), new AnalyzeDefvar());
    analyzers.put(getClSymbol("DEFPACKAGE"), (analyzer, form) -> {});
    analyzers.put(getClSymbol("DEFPARAMETER"), new AnalyzeDefparameter());
    analyzers.put(getClSymbol("DOLIST"), new AnalyzeDolist());
    analyzers.put(getClSymbol("ECASE"), new AnalyzeEcase());
    analyzers.put(getClSymbol("DESTRUCTURING-BIND"), new AnalyzeDestructuringBind());
    analyzers.put(getClSymbol("IN-PACKAGE"), new AnalyzeInPackage());
    analyzers.put(getClSymbol("LET"), new AnalyzeLet());
    analyzers.put(getClSymbol("LET*"), new AnalyzeLetStar());
    analyzers.put(getClSymbol("LOOP"), new AnalyzeLoop());
  }

  public void analyze() {
    // We can't parse files without knowing how packages are defined, and we don't want to deal with dependencies and
    // ordering which needs knowledge of ASDF or whatever system management is used. For a good enough approximation,
    // we first scan all the files for toplevel DEFPACKAGE usages, assuming that the DEFPACKAGE symbol is always
    // correct, and we scan everything else in a second pass, using strict symbol packages.
    analyzePackages();
    analyzeUsages();
    annotateSymbols();
  }

  private void analyzePackages() {
    Collection<VirtualFile> virtualFiles =
        FileTypeIndex.getFiles(LispFileType.INSTANCE, GlobalSearchScope.allScope(project));
    for (VirtualFile virtualFile : virtualFiles) {
      LispFile lispFile = (LispFile) PsiManager.getInstance(project).findFile(virtualFile);
      if (lispFile != null) {
        lispFile.getSexpList().forEach(this::checkDefpackage);
      }
    }
  }

  private void checkDefpackage(LispSexp sexp) {
    LispList form = sexp.getList();
    if (form == null) return;
    List<LispSexp> sexpList = form.getSexpList();
    if (sexpList.size() < 1) return;
    LispSymbol symbol = sexpList.get(0).getSymbol();
    if (symbol == null) return;
    if (symbol.getText().equals("defpackage")) {
      ANALYZE_DEFPACKAGE.analyze(this, form);
    }
  }

  private void analyzeUsages() {
    Collection<VirtualFile> virtualFiles =
        FileTypeIndex.getFiles(LispFileType.INSTANCE, GlobalSearchScope.allScope(project));
    for (VirtualFile virtualFile : virtualFiles) {
      LispFile lispFile = (LispFile) PsiManager.getInstance(project).findFile(virtualFile);
      if (lispFile != null) {
        analyzeForms(lispFile.getSexpList(), 0);
      }
    }
  }

  public String getStringDesignator(LispSexp nameDesignator) {
    if (nameDesignator.getList() != null) return null;
    LispSymbol symbolName = nameDesignator.getSymbol();
    if (symbolName != null) {
      highlightConstant(symbolName);
      Symbol symbol = symbolManager.getSymbol(symbolName.getText());
      return symbol.getName();
    }
    ASTNode token = nameDesignator.getFirstChild().getNode();
    if (token.getElementType() == STRING) {
      String text = token.getText();
      return text.substring(1, text.length() - 1);
    }
    return null;
  }

  private void annotateSymbols() {
    if (!lexicalBindings.isEmpty()) throw new RuntimeException("Unbalanced lexical stack.");
    lexicalBindings.getRetired().forEach(this::checkBinding);
    symbolManager.getFunctions().forEach(this::checkBinding);
    symbolManager.getVariables().forEach(this::checkBinding);
  }

  private void checkBinding(SymbolBinding binding) {
    checkNoUsages(binding);
    checkNoDefinition(binding);
  }

  private void checkNoDefinition(SymbolBinding symbolBinding) {
    if (!symbolBinding.isDefined()) {
      String message = symbolBinding.getSymbolType() == FUNCTION ? "Function '%s' does not exist" : "Variable '%s' is not defined";
      symbolBinding.getUsages().forEach(usage ->
          highlightUnknown(usage, String.format(message, symbolBinding.getName())));
    }
  }

  private void checkNoUsages(SymbolBinding symbolBinding) {
    if (symbolBinding.getUsages().isEmpty() && symbolBinding.getDefinition() != null) {
      String message = symbolBinding.getSymbolType() == FUNCTION ? "Function '%s' is never called" : "Variable '%s' is never used";
      highlightUnused(symbolBinding.getDefinition(), String.format(message, symbolBinding.getName()));
    }
  }

  void analyzeForms(Collection<LispSexp> forms, int skip) {
    forms.stream().skip(skip).forEach(this::analyzeForm);
  }

  void analyzeForm(LispSexp form) {
    LispSymbol symbol = form.getSymbol();
    if (symbol != null) {
      if (symbol.getText().startsWith(":")) {
        highlightConstant(symbol);
      } else {
        lexicalBindings.registerVariableUsage(symbol);
      }
    }
    LispList list = form.getList();
    if (list != null) {
      analyzeCompoundForm(list);
    }
    LispQuoted quoted = form.getQuoted();
    if (quoted != null) {
      highlightConstant(quoted);
    }
  }

  private void analyzeCompoundForm(LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.isEmpty()) return;
    LispSexp sexp0 = list.get(0);
    LispSymbol symbol0 = sexp0.getSymbol();
    if (symbol0 != null) {
      Symbol symbol = symbolManager.getSymbol(symbol0.getText());
      getAnalyzer(symbol).analyze(this, form);
    } else {
      // TODO: handle lambda expression case.
    }
  }

  private Analyzer getAnalyzer(Symbol symbol) {
    Analyzer analyzer = analyzers.get(symbol);
    return analyzer == null ? ANALYZE_FUNCTION_CALL : analyzer;
  }

  void highlightKeyword(LispList form) {
    highlightKeyword(form.getSexpList().get(0));
  }

  void highlightKeyword(PsiElement psiElement) {
    highlight(psiElement, KEYWORD);
  }

  void highlightConstant(PsiElement psiElement) {
    highlight(psiElement, CONSTANT);
  }

  void highlightUnknown(PsiElement psiElement, String message) {
    annotate(psiElement, HighlightSeverity.ERROR, message, WRONG_REFERENCES_ATTRIBUTES);
  }

  void highlightUnused(PsiElement psiElement, String message) {
    annotate(psiElement, HighlightSeverity.WARNING, message, NOT_USED_ELEMENT_ATTRIBUTES);
  }

  void highlight(PsiElement psiElement, TextAttributesKey constant) {
    silentlyAnnotate(psiElement, INFORMATION, constant);
  }

  void highlightError(PsiElement psiElement, String message) {
    if (psiElement.getContainingFile() != lispFile) return;
    holder.newAnnotation(HighlightSeverity.ERROR, message)
        .range(psiElement)
        .create();
  }
  private void silentlyAnnotate(PsiElement psiElement, HighlightSeverity severity, TextAttributesKey attributes) {
    if (psiElement.getContainingFile() != lispFile) return;
    holder.newSilentAnnotation(severity)
        .range(psiElement)
        .textAttributes(attributes)
        .create();
  }

  private void annotate(PsiElement psiElement, HighlightSeverity severity, String message, TextAttributesKey attributes) {
    if (psiElement.getContainingFile() != lispFile) return;
    holder.newAnnotation(severity, message)
        .textAttributes(attributes)
        .range(psiElement)
        .create();
  }

  private Symbol getClSymbol(String name) {
    return symbolManager.getPackage("CL").intern(name);
  }
}
