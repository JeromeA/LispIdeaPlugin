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
      clSymbol("DEFPARAMETER"), new AnalyzeDefvar(AnalyzeDefvar.Type.DEFPARAMETER),
      clSymbol("DEFSTRUCT"), new AnalyzeDefstruct(),
      clSymbol("DEFUN"), new AnalyzeDefun(AnalyzeDefun.Type.DEFUN),
      clSymbol("DEFVAR"), new AnalyzeDefvar(AnalyzeDefvar.Type.DEFVAR),
      clSymbol("DOLIST"), new AnalyzeDolist(),
      clSymbol("ECASE"), new AnalyzeEcase(),
      clSymbol("DESTRUCTURING-BIND"), new AnalyzeDestructuringBind(),
      clSymbol("IN-PACKAGE"), new AnalyzeInPackage(),
      clSymbol("LABELS"), new AnalyzeLabels(),
      clSymbol("LAMBDA"), ANALYZE_LAMBDA,
      clSymbol("LET"), new AnalyzeLet(),
      clSymbol("LET*"), new AnalyzeLetStar(),
      clSymbol("LOOP"), new AnalyzeLoop());

  private AnalysisContext context;
  private final LispFile lispFile;
  public final Set<PackageDefinition> scannedPackages = new HashSet<>();
  public List<String> completions = new ArrayList<>();

  public SyntaxAnalyzer(LispFile lispFile) {
    this.lispFile = lispFile;
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
    context.highlighter.highlightKeyword(quote);
    String quoteType = quote.getText();
    switch (quoteType) {
      case "'":
        // We arbitrarily decide to highlight quoted expressions as data.
        context.highlighter.highlightConstant(quoted);
        break;
      case "#'":
        LispSymbol symbolName = quotedSexp.getSymbol();
        if (symbolName != null) {
          context.addFunctionUsage(symbolName);
          return;
        }
        LispList list = quotedSexp.getList();
        if (list != null) {
          List<LispSexp> sexpList = list.getSexpList();
          if (sexpList.size() < 2
              || sexpList.get(0).getSymbol() == null
              || !sexpList.get(0).getSymbol().getText().equals("lambda")) {
            context.highlighter.highlightError(quotedSexp, "Expected lambda form");
            return;
          }
          ANALYZE_LAMBDA.analyze(context, list);
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
      completions.addAll(context.lexicalBindings.getLexicalVariables());
      completions.addAll(getGlobalVariableNames());
    } else {
      Symbol symbol = context.packageManager.getSymbol(parsedSymbol);
      if (symbol.isConstant()) {
        context.highlighter.highlightConstant(parsedSymbol);
      } else {
        context.addVariableUsage(parsedSymbol);
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
        completions.addAll(context.lexicalBindings.getLexicalFunctions());
        completions.addAll(getGlobalFunctions());
      } else {
        Symbol symbol = context.packageManager.getSymbol(parsedSymbol);
        getAnalyzer(symbol).analyze(context, form);
      }
    } else {
      // TODO: handle lambda expression case.
    }
  }

  private static synchronized FormAnalyzer getAnalyzer(Symbol symbol) {
    FormAnalyzer formAnalyzer = ANALYSERS.get(symbol);
    return formAnalyzer == null ? ANALYZE_FUNCTION_CALL : formAnalyzer;
  }

  private List<String> getGlobalVariableNames() {
    return ProjectComputedData.getInstance(lispFile.getProject()).getProjectAnalysis()
        .getVariables().stream()
        .map(SymbolBinding::getName)
        .collect(Collectors.toList());
  }

  /** These are really the global functions, not just the ones found by this analysis so far. */
  private List<String> getGlobalFunctions() {
    return ProjectComputedData.getInstance(lispFile.getProject()).getProjectAnalysis()
        .getFunctions().stream()
        .map(SymbolBinding::getName)
        .collect(Collectors.toList());
  }

  private boolean isCompletion(LispSymbol symbol) {
    return symbol.getText().endsWith(DUMMY_IDENTIFIER_TRIMMED);
  }

  public void setContext(AnalysisContext context) {
    this.context = context;
  }
}
