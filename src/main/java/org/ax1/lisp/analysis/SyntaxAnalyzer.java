package org.ax1.lisp.analysis;

import com.intellij.psi.PsiElement;
import org.apache.groovy.util.Maps;
import org.ax1.lisp.analysis.form.*;
import org.ax1.lisp.analysis.symbol.*;
import org.ax1.lisp.psi.*;
import org.ax1.lisp.subprocess.SubprocessFeatures;
import org.jetbrains.annotations.NotNull;

import java.util.*;
import java.util.stream.Collectors;

import static com.google.common.collect.ImmutableList.toImmutableList;
import static com.intellij.codeInsight.completion.CompletionUtilCore.DUMMY_IDENTIFIER_TRIMMED;
import static org.ax1.lisp.analysis.symbol.Symbol.clSymbol;
import static org.ax1.lisp.parsing.LispSyntaxHighlighter.COMMENT;
import static org.ax1.lisp.parsing.LispSyntaxHighlighter.READER_MACRO;

public class SyntaxAnalyzer {

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

  private AnalysisContext context;
  private final LispFile lispFile;

  public SyntaxAnalyzer(LispFile lispFile) {
    this.lispFile = lispFile;
  }

  public void analyze() {
    if (context.highlighter.getHolder() != null) {
      analyzeFeatureExpressions(lispFile.getPrefixedSexpList());
    }
    analyzeForms(lispFile.getSexpList(), 0);
  }

  public void analyzeForms(Collection<LispSexp> forms, int skip) {
    forms.stream().skip(skip).forEach(this::analyzeForm);
  }

  public void analyzeForm(LispSexp form) {
    if (form.isSymbol()) {
      analyseSymbolForm(form);
    }
    LispList list = form.getList();
    if (list != null) {
      analyzeCompoundForm(list);
    }
    if (form.getQuoted() != null) {
      analyseQuotedForm(form.getQuoted());
    }
  }

  public void analyzeFeatureExpressions(@NotNull List<LispPrefixedSexp> list) {
    list.forEach(this::analyseFeatureExpression);
  }

  private void analyseFeatureExpression(LispPrefixedSexp prefixedSexp) {
    List<LispReaderFeature> readerFeatureList = prefixedSexp.getReaderFeatureList();
    LispSexp sexp = prefixedSexp.getSexp();
    if (!readerFeatureList.isEmpty()) {
      readerFeatureList.forEach(feature -> context.highlighter.highlight(feature, READER_MACRO));
      if (!SubprocessFeatures.getInstance(lispFile.getProject()).eval(readerFeatureList.get(0))) {
        context.highlighter.highlight(sexp, COMMENT);
      }
    }
    LispList list = sexp.getList();
    LispQuoted quoted = sexp.getQuoted();
    if (quoted != null) {
      list = quoted.getSexp().getList();
    }
    if (list != null) {
      analyzeFeatureExpressions(list.getPrefixedSexpList());
    }
  }

  private void analyseQuotedForm(LispQuoted quoted) {
    PsiElement quote = quoted.getFirstChild();
    LispSexp quotedSexp = quoted.getSexp();
    context.highlighter.highlightKeyword(quote);
    String quoteType = quote.getText();
    switch (quoteType) {
      case "#'":
        if (quotedSexp.isSymbol()) {
          context.addFunctionUsage(quotedSexp.getSymbol());
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
      case ",":
      case ",@":
        context.highlighter.highlightError(quote, "Unexpected comma outside backquote expression");
        break;
      case "'":
        ANALYZE_QUOTE.analyze(context, quoted.getSexp());
        break;
      case "`":
        ANALYZE_BACKQUOTE.analyze(context, quoted.getSexp());
        break;
    }
  }

  private void analyseSymbolForm(LispSexp sexp) {
    Symbol symbol = context.getSymbol(sexp.getSymbol());
    if (symbol.isConstant()) {
      context.highlighter.highlightConstant(sexp);
    } else {
      context.addVariableUsage(sexp.getSymbol());
    }
  }

  private void analyzeCompoundForm(LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.isEmpty()) return;
    LispSexp sexp0 = list.get(0);
    if (sexp0.isSymbol()) {
      Symbol symbol = context.getSymbol(sexp0.getSymbol());
      context.addFunctionUsage(sexp0.getSymbol());
      getAnalyzer(symbol).analyze(context, form);
    } else if (isLambda(sexp0)){
      // TODO: handle lambda expression case.
    } else {
      context.highlighter.highlightError(sexp0, "Function name expected");
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
    return symbol0.getSymbolName().getValue().equals("LAMBDA");
  }

  private static Collection<String> toLowerCase(Collection<String> strings) {
    return strings.stream()
        .map(String::toLowerCase)
        .collect(toImmutableList());
  }

  private static synchronized FormAnalyzer getAnalyzer(Symbol symbol) {
    FormAnalyzer formAnalyzer = ANALYSERS.get(symbol);
    return formAnalyzer == null ? ANALYZE_FUNCTION_CALL : formAnalyzer;
  }

  public static boolean isCompletion(LispSexp sexp) {
    return sexp.getText().contains(DUMMY_IDENTIFIER_TRIMMED);
  }

  public static String getCompletionPrefix(LispSexp sexp) {
    String text = sexp.getText();
    int index = text.indexOf(DUMMY_IDENTIFIER_TRIMMED);
    return text.substring(0, index);
  }

  public void setContext(AnalysisContext context) {
    this.context = context;
  }
}
