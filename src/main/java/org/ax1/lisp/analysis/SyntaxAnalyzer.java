package org.ax1.lisp.analysis;

import com.intellij.psi.PsiElement;
import org.apache.groovy.util.Maps;
import org.ax1.lisp.analysis.form.*;
import org.ax1.lisp.analysis.symbol.*;
import org.ax1.lisp.psi.*;
import org.ax1.lisp.subprocess.SubprocessFeatures;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

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
      clSymbol("HANDLER-CASE"), new AnalyzeHandlerCase(),
      clSymbol("IN-PACKAGE"), new AnalyzeInPackage(),
      clSymbol("LABELS"), new AnalyzeLabels(),
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
  public List<String> completions = new ArrayList<>();

  public SyntaxAnalyzer(LispFile lispFile) {
    this.lispFile = lispFile;
  }

  public void analyze() {
    lispFile.getPrefixedSexpList().forEach(this::analysePrefixExpression);
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
      analyzeReaderExpressions(list);
      analyzeCompoundForm(list);
    }
    if (form.getQuoted() != null) {
      analyseQuotedForm(form.getQuoted());
    }
  }

  private void analyzeReaderExpressions(LispList list) {
    list.getPrefixedSexpList().forEach(this::analysePrefixExpression);
  }

  private void analysePrefixExpression(LispPrefixedSexp prefixedSexp) {
    @Nullable LispPrefix prefix = prefixedSexp.getPrefix();
    if (prefix == null) return;
    context.highlighter.highlight(prefix, READER_MACRO);
    @NotNull List<LispReaderFeature> readerFeatureList = prefix.getReaderFeatureList();
    if (!readerFeatureList.isEmpty() && !SubprocessFeatures.getInstance(lispFile.getProject()).eval(readerFeatureList.get(0))) {
      context.highlighter.highlight(prefixedSexp.getSexp(), COMMENT);
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
      case "`":
      case ",":
      case ",@":
        // We arbitrarily decide to highlight backquoted expressions as code.
        analyzeForm(quotedSexp);
        break;
    }
  }

  private void analyseSymbolForm(LispSexp sexp) {
    if (isCompletion(sexp)) {
      completions.addAll(toLowerCase(context.lexicalBindings.getLexicalVariables()));
      completions.addAll(toLowerCase(getGlobalVariableNames()));
      completions.addAll(getParentKeywords(sexp));
    } else {
      Symbol symbol = context.getSymbol(sexp.getSymbol());
      if (symbol.isConstant()) {
        context.highlighter.highlightConstant(sexp);
      } else {
        context.addVariableUsage(sexp.getSymbol());
      }
    }
  }

  private Collection<String> getParentKeywords(LispSexp sexp) {
    PsiElement parent = sexp.getParent();
    if (!(parent instanceof LispList)) return List.of();
    LispList form = (LispList) parent;
    LispSexp sexp0 = form.getSexpList().get(0);
    Symbol symbol = context.getSymbol(sexp0.getSymbol());
    SymbolDefinition symbolDefinition = ProjectComputedData.getInstance(lispFile.getProject()).getProjectAnalysis()
        .getFunction(symbol);
    if (symbolDefinition == null) return List.of();
    Lambda lambda = symbolDefinition.getLambda();
    if (lambda == null) return List.of();
    return lambda.keys.stream().map(name -> ":" + name).collect(Collectors.toUnmodifiableList());
  }

  private void analyzeCompoundForm(LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.isEmpty()) return;
    LispSexp sexp0 = list.get(0);
    if (sexp0.isSymbol()) {
      if (isCompletion(sexp0)) {
        completions.addAll(toLowerCase(context.lexicalBindings.getLexicalFunctions()));
        completions.addAll(toLowerCase(getGlobalFunctions()));
      } else {
        Symbol symbol = context.getSymbol(sexp0.getSymbol());
        context.addFunctionUsage(sexp0.getSymbol());
        getAnalyzer(symbol).analyze(context, form);
      }
    } else {
      // TODO: handle lambda expression case.
    }
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

  private List<String> getGlobalVariableNames() {
    return ProjectComputedData.getInstance(lispFile.getProject()).getProjectAnalysis()
        .getVariables().stream()
        .map(SymbolDefinition::getName)
        .collect(Collectors.toList());
  }

  /** These are really the global functions, not just the ones found by this analysis so far. */
  private List<String> getGlobalFunctions() {
    return ProjectComputedData.getInstance(lispFile.getProject()).getProjectAnalysis()
        .getFunctions().stream()
        .map(SymbolDefinition::getName)
        .collect(Collectors.toList());
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
