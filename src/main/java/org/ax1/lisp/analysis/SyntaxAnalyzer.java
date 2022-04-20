package org.ax1.lisp.analysis;

import com.intellij.lang.ASTNode;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import org.apache.groovy.util.Maps;
import org.ax1.lisp.analysis.AnalyzeDefun.Type;
import org.ax1.lisp.psi.*;
import org.jetbrains.annotations.NotNull;

import java.util.*;
import java.util.stream.Stream;

import static com.intellij.lang.annotation.HighlightSeverity.INFORMATION;
import static com.intellij.openapi.editor.DefaultLanguageHighlighterColors.CONSTANT;
import static com.intellij.openapi.editor.colors.CodeInsightColors.NOT_USED_ELEMENT_ATTRIBUTES;
import static com.intellij.openapi.editor.colors.CodeInsightColors.WRONG_REFERENCES_ATTRIBUTES;
import static org.ax1.lisp.analysis.SymbolBinding.SymbolType.FUNCTION;
import static org.ax1.lisp.parsing.LispSyntaxHighlighter.KEYWORD;
import static org.ax1.lisp.psi.LispTypes.STRING;

public class SyntaxAnalyzer {

  // Names that could look like function calls, but are really not.
  private static final Set<String> KEYWORDS =
      Set.of("declare", "if", "ignore", "return", "setq", "special", "unless", "when");

  private static final Map<String, Analyzer> ANALYZERS = Maps.of(
      "cond", new AnalyzeCond(),
      "defmacro", new AnalyzeDefun(Type.DEFMACRO),
      "defun", new AnalyzeDefun(Type.DEFUN),
      "defvar", new AnalyzeDefvar(),
      "defpackage", new AnalyzeDefpackage(),
      "defparameter", new AnalyzeDefparameter(),
      "dolist", new AnalyzeDolist(),
      "ecase", new AnalyzeEcase(),
      "destructuring-bind", new AnalyzeDestructuringBind(),
      "in-package", new AnalyzeInPackage(),
      "let", new AnalyzeLet(),
      "let*", new AnalyzeLetStar(),
      "loop", new AnalyzeLoop());

  private final LispFile lispFile;
  private final AnnotationHolder holder;
  final PackageManager packages;
  final DynamicSymbolManager dynamicSymbols;
  final LexicalSymbolManager lexicalSymbols;
  String packageName = "cl-user";

  public SyntaxAnalyzer(LispFile lispFile, @NotNull AnnotationHolder holder) {
    this.lispFile = lispFile;
    this.holder = holder;
    Project project = lispFile.getProject();
    this.packages = PackageManager.getInstance(project);
    this.dynamicSymbols = DynamicSymbolManager.getInstance(project);
    this.lexicalSymbols = new LexicalSymbolManager(dynamicSymbols);
  }

  public String getStringDesignator(LispSexp nameDesignator) {
    if (nameDesignator.getList() != null) return null;
    LispSymbol symbol = nameDesignator.getSymbol();
    if (symbol != null) {
      highlightConstant(symbol);
      String text = symbol.getText();
      int colonIndex = text.indexOf(':');
      if (colonIndex >= 0) {
        return text.substring(colonIndex + 1);
      }
      return text;
    }
    ASTNode token = nameDesignator.getFirstChild().getNode();
    if (token.getElementType() == STRING) {
      String text = token.getText();
      return text.substring(1, text.length() - 1);
    }
    return null;
  }

  public void analyze() {
    analyzeForms(lispFile.getSexpList(), 0);
    annotateSymbols();
  }

  private void annotateSymbols() {
    if (!lexicalSymbols.isEmpty()) throw new RuntimeException("Unbalanced lexical stack.");
    for (SymbolBinding symbolBinding : lexicalSymbols.getRetired()) {
      checkNoUsages(symbolBinding);
    }
    dynamicSymbols.getSymbols().values().stream()
        .flatMap(descriptor -> Stream.of(descriptor.getFunction(), descriptor.getVariable()))
        .filter(binding -> binding.getDefinition() != null || !binding.getUsages().isEmpty())
        .forEach(binding -> {
          checkNoUsages(binding);
          checkNoDefinition(binding);
        });
  }

  private void checkNoDefinition(SymbolBinding symbolBinding) {
    if (symbolBinding.getDefinition() == null
        && !getPackage().isSymbol(symbolBinding.getSymbolType(), symbolBinding.getName())) {
      String message = symbolBinding.getSymbolType() == FUNCTION ? "Function '%s' does not exist" : "Variable '%s' is not defined";
      symbolBinding.getUsages().forEach(usage ->
          highlightUnknown(usage, String.format(message, symbolBinding.getName())));
    }
  }

  private void checkNoUsages(SymbolBinding symbolBinding) {
    if (symbolBinding.getUsages().isEmpty()) {
      String message = symbolBinding.getSymbolType() == FUNCTION ? "Function '%s' is never called" : "Variable '%s' is never used";
      highlightUnused(symbolBinding.getDefinition(), String.format(message, symbolBinding.getName()));
    }
  }

  private Package getPackage() {
    return packages.get(packageName);
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
        lexicalSymbols.registerVariableUsage(symbol);
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
      String symbolName = symbol0.getText();
      Analyzer analyzer = ANALYZERS.get(symbolName);
      if (analyzer != null) {
        analyzer.analyze(this, form);
      } else {
        analyzeFunctionCall(symbol0, form);
      }
    } else {
      // TODO: handle lambda expression case.
    }
  }

  private void analyzeFunctionCall(LispSymbol functionName, LispList form) {
    if (KEYWORDS.contains(functionName.getName())) {
      highlightKeyword(functionName);
    }
    lexicalSymbols.registerFunctionUsage(functionName);
    analyzeForms(form.getSexpList(), 1);
  }

  void highlightError(PsiElement psiElement, String message) {
    holder.newAnnotation(HighlightSeverity.ERROR, message)
        .range(psiElement)
        .create();
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
    if (psiElement.getContainingFile() != lispFile) return;
    holder.newAnnotation(HighlightSeverity.ERROR, message)
        .textAttributes(WRONG_REFERENCES_ATTRIBUTES)
        .range(psiElement)
        .create();
  }

  void highlightUnused(PsiElement psiElement, String message) {
    if (psiElement.getContainingFile() != lispFile) return;
    holder.newAnnotation(HighlightSeverity.WARNING, message)
        .textAttributes(NOT_USED_ELEMENT_ATTRIBUTES)
        .range(psiElement)
        .create();
  }

  void highlight(PsiElement psiElement, TextAttributesKey constant) {
    holder.newSilentAnnotation(INFORMATION)
        .range(psiElement)
        .textAttributes(constant)
        .create();
  }
}
