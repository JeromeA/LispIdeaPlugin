package org.ax1.lisp.analysis;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.psi.PsiElement;
import org.apache.groovy.util.Maps;
import org.ax1.lisp.psi.*;
import org.jetbrains.annotations.NotNull;

import java.util.*;

import static com.intellij.lang.annotation.HighlightSeverity.INFORMATION;
import static com.intellij.openapi.editor.DefaultLanguageHighlighterColors.CONSTANT;
import static com.intellij.openapi.editor.colors.CodeInsightColors.NOT_USED_ELEMENT_ATTRIBUTES;
import static com.intellij.openapi.editor.colors.CodeInsightColors.WRONG_REFERENCES_ATTRIBUTES;
import static org.ax1.lisp.analysis.SymbolDescriptor.SymbolType.FUNCTION;
import static org.ax1.lisp.analysis.SymbolDescriptor.SymbolType.VARIABLE;
import static org.ax1.lisp.parsing.LispSyntaxHighlighter.KEYWORD;

public class SyntaxAnalyzer {

  // Names that could look like function calls, but are really not.
  private static final Set<String> KEYWORDS =
      Set.of("declare", "if", "ignore", "return", "setq", "special", "unless", "when");

  private static final Map<String, Analyzer> ANALYZERS = Maps.of(
      "cond", new AnalyzeCond(),
      "defun", new AnalyzeDefun(),
      "defvar", new AnalyzeDefvar(),
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
  String packageName = "cl-user";
  final SymbolStack functions = new SymbolStack(FUNCTION);
  final SymbolStack variables = new SymbolStack(VARIABLE);
  final PackageManager packages;

  public SyntaxAnalyzer(LispFile lispFile, @NotNull AnnotationHolder holder) {
    this.lispFile = lispFile;
    this.holder = holder;
    this.packages = PackageManager.getInstance(lispFile.getProject());
  }

  public void analyze() {
    analyzeForms(lispFile.getSexpList(), 0);
    annotateSymbols(functions);
    annotateSymbols(variables);
  }

  private void annotateSymbols(SymbolStack symbols) {
    if (!symbols.getLexical().empty()) throw new RuntimeException("Unbalanced lexical stack.");
    for (SymbolDescriptor symbolDescriptor : symbols.getRetired()) {
      checkNoUsages(symbolDescriptor);
    }
    symbols.getSpecial().values().forEach(symbolDescriptor -> {
      checkNoUsages(symbolDescriptor);
      checkNoDefinition(symbolDescriptor);
    });
  }

  private void checkNoDefinition(SymbolDescriptor symbolDescriptor) {
    if (symbolDescriptor.getDefinition() == null
        && !getPackage().isSymbol(symbolDescriptor.getSymbolType(), symbolDescriptor.getName())) {
      String message = symbolDescriptor.getSymbolType() == FUNCTION ? "Function '%s' does not exist" : "Variable '%s' is not defined";
      symbolDescriptor.getUsages().forEach(usage ->
          highlightUnknown(usage, String.format(message, symbolDescriptor.getName())));
    }
  }

  private void checkNoUsages(SymbolDescriptor symbolDescriptor) {
    if (symbolDescriptor.getUsages().isEmpty()) {
      String message = symbolDescriptor.getSymbolType() == FUNCTION ? "Function '%s' is never called" : "Variable '%s' is never used";
      holder.newAnnotation(HighlightSeverity.WARNING,
              String.format(message, symbolDescriptor.getName()))
          .textAttributes(NOT_USED_ELEMENT_ATTRIBUTES)
          .range(symbolDescriptor.getDefinition())
          .create();
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
        highlight(symbol, CONSTANT);
      } else {
        variables.registerUsage(symbol);
      }
    }
    LispList list = form.getList();
    if (list != null) {
      analyzeCompoundForm(list);
    }
    LispQuoted quoted = form.getQuoted();
    if (quoted != null) {
      highlight(quoted, CONSTANT);
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
      highlight(functionName, KEYWORD);
    }
    functions.registerUsage(functionName);
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

  void highlightUnknown(PsiElement psiElement, String message) {
    holder.newAnnotation(HighlightSeverity.ERROR, message)
        .textAttributes(WRONG_REFERENCES_ATTRIBUTES)
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
