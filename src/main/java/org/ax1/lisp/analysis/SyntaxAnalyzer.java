package org.ax1.lisp.analysis;

import com.intellij.psi.PsiElement;
import org.apache.groovy.util.Maps;
import org.ax1.lisp.analysis.form.*;
import org.ax1.lisp.analysis.symbol.*;
import org.ax1.lisp.psi.*;

import java.util.*;
import java.util.stream.Collectors;

import static com.intellij.codeInsight.completion.CompletionUtilCore.DUMMY_IDENTIFIER_TRIMMED;
import static org.ax1.lisp.analysis.symbol.Symbol.clSymbol;

public class SyntaxAnalyzer {

  private static final AnalyzeFunctionCall ANALYZE_FUNCTION_CALL = new AnalyzeFunctionCall();
  private static final AnalyzeLambda ANALYZE_LAMBDA = new AnalyzeLambda();

  private static final Map<Symbol, FormAnalyzer> ANALYSERS = Maps.of(
      clSymbol("COND"), new AnalyzeCond(),
      clSymbol("DEFCLASS"), new AnalyzeDefclass(),
      clSymbol("DEFGENERIC"), new AnalyzeDefgeneric(),
      clSymbol("DEFMACRO"), new AnalyzeDefun(AnalyzeDefun.Type.DEFMACRO),
      clSymbol("DEFMETHOD"), new AnalyzeDefmethod(),
      clSymbol("DEFPACKAGE"), new AnalyzeDefpackage(),
      clSymbol("DEFPARAMETER"), new AnalyzeDefparameter(),
      clSymbol("DEFSTRUCT"), new AnalyzeDefstruct(),
      clSymbol("DEFUN"), new AnalyzeDefun(AnalyzeDefun.Type.DEFUN),
      clSymbol("DEFVAR"), new AnalyzeDefvar(),
      clSymbol("DOLIST"), new AnalyzeDolist(),
      clSymbol("ECASE"), new AnalyzeEcase(),
      clSymbol("DESTRUCTURING-BIND"), new AnalyzeDestructuringBind(),
      clSymbol("IN-PACKAGE"), new AnalyzeInPackage(),
      clSymbol("LABELS"), new AnalyzeLabels(),
      clSymbol("LAMBDA"), ANALYZE_LAMBDA,
      clSymbol("LET"), new AnalyzeLet(),
      clSymbol("LET*"), new AnalyzeLetStar(),
      clSymbol("LOOP"), new AnalyzeLoop());

  private final LispFile lispFile;
  public final PackageManager packageManager;
  public final Set<PackageDefinition> scannedPackages = new HashSet<>();
  public final LexicalBindingManager lexicalBindings;
  public List<String> completions = new ArrayList<>();
  public final Annotate annotations;

  public SyntaxAnalyzer(LispFile lispFile, Annotate annotations, PackageManager packageManager) {
    this.lispFile = lispFile;
    this.annotations = annotations;
    this.packageManager = packageManager;
    lexicalBindings = new LexicalBindingManager(this);
  }

  public void analyze() {
    analyzeForms(lispFile.getSexpList(), 0);
  }

  public void analyzeForms(Collection<LispSexp> forms, int skip) {
    forms.stream().skip(skip).forEach(this::analyzeForm);
  }

  public void analyzeForm(LispSexp form) {
    if (form.getSymbol() != null) {
      analyseSymbolForm(form.getSymbol());
    }
    if (form.getList() != null) {
      analyzeCompoundForm(form.getList());
    }
    if (form.getQuoted() != null) {
      analyseQuotedForm(form.getQuoted());
    }
  }

  private void analyseQuotedForm(LispQuoted quoted) {
    PsiElement quote = quoted.getFirstChild();
    LispSexp quotedSexp = quoted.getSexp();
    annotations.highlightKeyword(quote);
    String quoteType = quote.getText();
    switch (quoteType) {
      case "'":
        // We arbitrarily decide to highlight quoted expressions as data.
        annotations.highlightConstant(quoted);
        break;
      case "#'":
        LispSymbol parsedSymbol = quotedSexp.getSymbol();
        if (parsedSymbol != null) {
          Symbol symbol = packageManager.getSymbol(parsedSymbol);
          lexicalBindings.registerFunctionUsage(symbol, parsedSymbol);
          return;
        }
        LispList list = quotedSexp.getList();
        if (list != null) {
          List<LispSexp> sexpList = list.getSexpList();
          if (sexpList.size() < 2
              || sexpList.get(0).getSymbol() == null
              || !sexpList.get(0).getSymbol().getText().equals("lambda")) {
            annotations.highlightError(quotedSexp, "Expected lambda form");
            return;
          }
          ANALYZE_LAMBDA.analyze(this, list);
        }
        break;
      case "`":
      case ",":
      case ",@":
        // We arbitrarily decide to highlight backquoted expressions as code.
        analyzeForm(quotedSexp);
        break;
    }
  }

  private void analyseSymbolForm(LispSymbol parsedSymbol) {
    if (isCompletion(parsedSymbol)) {
      completions.addAll(lexicalBindings.getLexicalVariables());
      completions.addAll(getGlobalVariables());
    } else {
      Symbol symbol = packageManager.getSymbol(parsedSymbol);
      SymbolBinding binding = lexicalBindings.registerVariableUsage(symbol, parsedSymbol);
      if (binding.isKeyword()) {
        annotations.highlightConstant(parsedSymbol);
      }
    }
  }

  private void analyzeCompoundForm(LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.isEmpty()) return;
    LispSexp sexp0 = list.get(0);
    LispSymbol parsedSymbol = sexp0.getSymbol();
    if (parsedSymbol != null) {
      if (isCompletion(parsedSymbol)) {
        completions.addAll(lexicalBindings.getLexicalFunctions());
        completions.addAll(getGlobalFunctions());
      } else {
        Symbol symbol = packageManager.getSymbol(parsedSymbol);
        getAnalyzer(symbol).analyze(this, form);
      }
    } else {
      // TODO: handle lambda expression case.
    }
  }

  private static synchronized FormAnalyzer getAnalyzer(Symbol symbol) {
    FormAnalyzer formAnalyzer = ANALYSERS.get(symbol);
    return formAnalyzer == null ? ANALYZE_FUNCTION_CALL : formAnalyzer;
  }

  /** These are really the global variables, not just the ones found by this analysis so far. */
  private List<String> getGlobalVariables() {
    return ProjectAnalyser.getInstance(lispFile.getProject()).getProjectSymbolAnalysis()
        .variables.values().stream()
        .map(SymbolBinding::getSymbol)
        .map(Symbol::getName)
        .collect(Collectors.toList());
  }

  /** These are really the global functions, not just the ones found by this analysis so far. */
  private List<String> getGlobalFunctions() {
    return ProjectAnalyser.getInstance(lispFile.getProject()).getProjectSymbolAnalysis()
        .functions.values().stream()
        .map(SymbolBinding::getSymbol)
        .map(Symbol::getName)
        .collect(Collectors.toList());
  }

  private boolean isCompletion(LispSymbol symbol) {
    return symbol.getText().endsWith(DUMMY_IDENTIFIER_TRIMMED);
  }
}
