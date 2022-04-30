package org.ax1.lisp.analysis;

import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.analysis.symbol.SymbolManager;
import org.ax1.lisp.psi.*;

import java.util.*;

public class SyntaxAnalyzer {

  private static final AnalyzeFunctionCall ANALYZE_FUNCTION_CALL = new AnalyzeFunctionCall();

  private final Map<Symbol, Analyzer> analyzers = new HashMap<>();

  public final SymbolManager symbolManager;
  final LexicalBindingManager lexicalBindings;
  private LispFile lispFile;
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
      if (symbol.getText().startsWith(":")) {
        annotations.highlightConstant(symbol);
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
      annotations.highlightConstant(quoted);
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

  private Symbol getClSymbol(String name) {
    return symbolManager.getPackage("CL").intern(symbolManager, name);
  }
}
