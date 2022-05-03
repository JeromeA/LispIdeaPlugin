package org.ax1.lisp.analysis;

import com.intellij.psi.PsiElement;
import org.ax1.lisp.LispProject;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.analysis.symbol.SymbolBinding;
import org.ax1.lisp.analysis.symbol.SymbolManager;
import org.ax1.lisp.psi.*;

import java.util.*;
import java.util.stream.Collectors;

import static com.intellij.codeInsight.completion.CompletionUtilCore.DUMMY_IDENTIFIER_TRIMMED;

public class SyntaxAnalyzer {

  private static final AnalyzeFunctionCall ANALYZE_FUNCTION_CALL = new AnalyzeFunctionCall();

  private final Map<Symbol, Analyzer> analyzers = new HashMap<>();

  private final LispFile lispFile;
  public final SymbolManager symbolManager;
  public final LexicalBindingManager lexicalBindings;
  public List<String> completions = new ArrayList<>();
  final Annotate annotations;

  public SyntaxAnalyzer(LispFile lispFile, Annotate annotations, SymbolManager symbolManager) {
    this.lispFile = lispFile;
    this.annotations = annotations;
    this.symbolManager = symbolManager;
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
    analyzeForms(lispFile.getSexpList(), 0);
  }

  void analyzeForms(Collection<LispSexp> forms, int skip) {
    forms.stream().skip(skip).forEach(this::analyzeForm);
  }

  void analyzeForm(LispSexp form) {
    LispSymbol symbol = form.getSymbol();
    if (symbol != null) {
      if (isCompletion(symbol)) {
        completions.addAll(lexicalBindings.getLexicalVariables());
        completions.addAll(getGlobalVariables());
      } else {
        SymbolBinding binding = lexicalBindings.getVariableBinding(symbol.getText());
        if (binding.isKeyword()) {
          annotations.highlightConstant(symbol);
        } else {
          lexicalBindings.registerVariableUsage(symbol);
        }
      }
    }
    LispList list = form.getList();
    if (list != null) {
      analyzeCompoundForm(list);
    }
    LispQuoted quoted = form.getQuoted();
    if (quoted != null) {
      PsiElement quote = quoted.getFirstChild();
      annotations.highlightKeyword(quote);
      String quoteType = quote.getText();
      // Quoted expressions are data, backquoted expressions are code. These are arbitrary, it's just the most
      // common case.
      if (quoteType.equals("'")) {
        annotations.highlightConstant(quoted);
      } else {
        analyzeForm(quoted.getSexp());
      }
    }
  }

  private void analyzeCompoundForm(LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.isEmpty()) return;
    LispSexp sexp0 = list.get(0);
    LispSymbol symbol0 = sexp0.getSymbol();
    if (symbol0 != null) {
      if (isCompletion(symbol0)) {
        completions.addAll(lexicalBindings.getLexicalFunctions());
        completions.addAll(getGlobalFunctions());
      } else {
        Symbol symbol = symbolManager.getSymbol(symbol0.getText());
        getAnalyzer(symbol).analyze(this, form);
      }
    } else {
      // TODO: handle lambda expression case.
    }
  }

  private Analyzer getAnalyzer(Symbol symbol) {
    Analyzer analyzer = analyzers.get(symbol);
    return analyzer == null ? ANALYZE_FUNCTION_CALL : analyzer;
  }

  private Symbol getClSymbol(String name) {
    return symbolManager.getPackage("CL").intern(symbolManager, name);
  }

  private List<String> getGlobalVariables() {
    return LispProject.getInstance(lispFile.getProject()).getSymbolManager()
        .getAvailableVariables(symbolManager.getCurrentPackage()).stream()
        .map(Symbol::getName)
        .collect(Collectors.toList());
  }

  private List<String> getGlobalFunctions() {
    return LispProject.getInstance(lispFile.getProject()).getSymbolManager()
        .getAvailableFunctions(symbolManager.getCurrentPackage()).stream()
        .map(Symbol::getName)
        .collect(Collectors.toList());
  }

  private boolean isCompletion(LispSymbol symbol) {
    return symbol.getText().endsWith(DUMMY_IDENTIFIER_TRIMMED);
  }
}
