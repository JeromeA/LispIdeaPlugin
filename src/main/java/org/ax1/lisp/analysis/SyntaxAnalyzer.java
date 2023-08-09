package org.ax1.lisp.analysis;

import com.intellij.psi.PsiElement;
import com.intellij.psi.util.CachedValueProvider;
import com.intellij.psi.util.CachedValuesManager;
import org.apache.groovy.util.Maps;
import org.ax1.lisp.SymbolResolver;
import org.ax1.lisp.analysis.form.*;
import org.ax1.lisp.analysis.symbol.*;
import org.ax1.lisp.psi.*;

import java.util.*;

import static org.ax1.lisp.analysis.BaseLispElement.Type.*;
import static org.ax1.lisp.analysis.symbol.LexicalVariables.find;
import static org.ax1.lisp.analysis.symbol.Symbol.clSymbol;

public class SyntaxAnalyzer {

  public static final SyntaxAnalyzer INSTANCE = new SyntaxAnalyzer();
  private static final AnalyzeFunctionCall ANALYZE_FUNCTION_CALL = new AnalyzeFunctionCall();
  private static final AnalyzeLambda ANALYZE_LAMBDA = new AnalyzeLambda();
  private static final AnalyzeQuote ANALYZE_QUOTE = new AnalyzeQuote();
  private static final AnalyzeBackQuote ANALYZE_BACKQUOTE = new AnalyzeBackQuote();

  private static final Map<Symbol, FormAnalyzer> ANALYSERS = Maps.of(
      clSymbol("CASE"), new AnalyzeCase(),
      clSymbol("COND"), new AnalyzeCond(),
      clSymbol("DEFCLASS"), new AnalyzeDefclass(),
      clSymbol("DEFCONSTANT"), new AnalyzeDefvar(AnalyzeDefvar.Type.DEFCONSTANT),
      clSymbol("DEFGENERIC"), new AnalyzeDefgeneric(),
      clSymbol("DEFINE-CONDITION"), new AnalyzeDefineCondition(),
      clSymbol("DEFMACRO"), new AnalyzeDefun(AnalyzeDefun.Type.DEFMACRO),
      clSymbol("DEFMETHOD"), new AnalyzeDefmethod(),
      clSymbol("DEFPACKAGE"), new AnalyzeDefpackage(),
      clSymbol("DEFPARAMETER"), new AnalyzeDefvar(AnalyzeDefvar.Type.DEFPARAMETER),
      clSymbol("DEFSTRUCT"), new AnalyzeDefstruct(),
      clSymbol("DEFUN"), new AnalyzeDefun(AnalyzeDefun.Type.DEFUN),
      clSymbol("DEFVAR"), new AnalyzeDefvar(AnalyzeDefvar.Type.DEFVAR),
      clSymbol("DESTRUCTURING-BIND"), new AnalyzeDestructuringBind(),
      clSymbol("DO-ALL-SYMBOLS"), new AnalyzeDoSymbols(AnalyzeDoSymbols.Type.DO_ALL_SYMBOLS),
      clSymbol("DO-EXTERNAL-SYMBOLS"), new AnalyzeDoSymbols(AnalyzeDoSymbols.Type.DO_EXTERNAL_SYMBOLS),
      clSymbol("DO-SYMBOLS"), new AnalyzeDoSymbols(AnalyzeDoSymbols.Type.DO_SYMBOLS),
      clSymbol("DOLIST"), new AnalyzeDolist(),
      clSymbol("DOTIMES"), new AnalyzeDoTimes(),
      clSymbol("ECASE"), new AnalyzeEcase(),
      clSymbol("EVAL-WHEN"), new AnalyzeEvalWhen(),
      clSymbol("FLET"), new AnalyzeLabels(AnalyzeLabels.Type.FLET),
      clSymbol("HANDLER-BIND"), new AnalyzeHandlerBind(),
      clSymbol("HANDLER-CASE"), new AnalyzeHandlerCase(),
      clSymbol("IN-PACKAGE"), new AnalyzeInPackage(),
      clSymbol("LABELS"), new AnalyzeLabels(AnalyzeLabels.Type.LABELS),
      clSymbol("LAMBDA"), ANALYZE_LAMBDA,
      clSymbol("LET"), new AnalyzeLet(),
      clSymbol("LET*"), new AnalyzeLetStar(),
      clSymbol("LOOP"), new AnalyzeLoop(),
      clSymbol("MULTIPLE-VALUE-BIND"), new AnalyzeMultipleValueBind(),
      clSymbol("WITH-INPUT-FROM-STRING"), new AnalyzeWithInputFromString(),
      clSymbol("WITH-OPEN-FILE"), new AnalyzeWithOpenFile(),
      clSymbol("WITH-OUTPUT-TO-STRING"), new AnalyzeWithOutputToString());

  private SyntaxAnalyzer() {
  }

  public void analyze(LispFile lispFile) {
    CachedValuesManager.getCachedValue(lispFile, () -> {
      analyzeForms(lispFile.getSexpList(), 0);
      return CachedValueProvider.Result.create(Boolean.TRUE, lispFile);
    });

  }

  public void analyzeForms(Collection<LispSexp> forms, int skip) {
    forms.stream().skip(skip).forEach(this::analyzeForm);
  }

  public void analyzeFormsWithVariables(Collection<LispSexp> forms, int skip, Collection<LexicalVariable> variables) {
    forms.stream().skip(skip).forEach(f -> f.addLexicalVariables(variables));
    forms.stream().skip(skip).forEach(this::analyzeForm);
  }

  public void analyzeFormsWithFunctions(Collection<LispSexp> forms, int skip, Collection<LispSymbolName> functions) {
    forms.stream().skip(skip).forEach(f -> f.addLexicalFunctions(functions));
    forms.stream().skip(skip).forEach(this::analyzeForm);
  }

  public void analyzeForm(LispSexp form) {
    if (form.isSymbol()) {
      LispSymbolName symbolName = form.getSymbolName();
      LexicalVariable lexicalVariable = find(symbolName);
      if (lexicalVariable != null) {
        symbolName.setType(LEXICAL_VARIABLE_USAGE);
        symbolName.setLexicalVariable(lexicalVariable);
        lexicalVariable.usages.add(symbolName);
      } else {
        symbolName.setType(VARIABLE_USAGE);
      }
      return;
    }
    if (form.getQuoted() != null) {
      analyseQuotedForm(form.getQuoted());
      return;
    }
    LispList list = form.getList();
    if (list != null) {
      analyzeCompoundForm(list);
      return;
    }
    LispString string = form.getString();
    if (string != null) {
      string.getStringContent().setType(DATA);
    }
  }

  private void analyseQuotedForm(LispQuoted quoted) {
    PsiElement quote = quoted.getFirstChild();
    LispSexp quotedSexp = quoted.getSexp();
    String quoteType = quote.getText();
    switch (quoteType) {
      case "#'":
        if (quotedSexp.isSymbol()) {
          quotedSexp.getSymbolName().setType(FUNCTION_DEFINITION);
          return;
        }
        LispList list = quotedSexp.getList();
        if (list != null) {
          List<LispSexp> sexpList = list.getSexpList();
          if (sexpList.size() < 2
              || sexpList.get(0).getSymbol() == null
              || !sexpList.get(0).getSymbol().getText().equals("lambda")) {
            quotedSexp.setErrorMessage("Expected lambda form");
            return;
          }
          ANALYZE_LAMBDA.analyze(list);
        }
        break;
      case ",":
      case ",@":
//        context.highlighter.highlightError(quote, "Unexpected comma outside backquote expression");
        break;
      case "'":
        ANALYZE_QUOTE.analyze(quoted.getSexp());
        break;
      case "`":
        ANALYZE_BACKQUOTE.analyze(quoted.getSexp());
        break;
    }
  }

  private void analyzeCompoundForm(LispList form) {
    form.setType(CODE);
    List<LispSexp> list = form.getSexpList();
    if (list.isEmpty()) return;
    LispSexp sexp0 = list.get(0);
    if (sexp0.isSymbol()) {
      sexp0.getSymbolName().setType(FUNCTION_USAGE);
      Symbol symbol = SymbolResolver.resolve(sexp0.getSymbolName());
      getAnalyzer(symbol).analyze(form);
    } else if (isLambda(sexp0)) {
      // TODO: handle lambda expression case.
    } else {
      sexp0.setErrorMessage("Function name expected");
    }
  }

  private boolean isLambda(LispSexp sexp) {
    LispList list = sexp.getList();
    if (list == null) return false;
    List<LispSexp> sexpList = list.getSexpList();
    if (sexpList.isEmpty()) return false;
    LispSexp sexp0 = sexpList.get(0);
    LispSymbol symbol0 = sexp0.getSymbol();
    if (symbol0 == null) return false;
    return symbol0.getSymbolName().getLispName().equals("LAMBDA");
  }

  private static synchronized FormAnalyzer getAnalyzer(Symbol symbol) {
    FormAnalyzer formAnalyzer = ANALYSERS.get(symbol);
    return formAnalyzer == null ? ANALYZE_FUNCTION_CALL : formAnalyzer;
  }

}
