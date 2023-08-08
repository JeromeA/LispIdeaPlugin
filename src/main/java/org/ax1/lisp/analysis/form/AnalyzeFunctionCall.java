package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbolName;

import java.util.Set;

import static org.ax1.lisp.analysis.BaseLispElement.Type.KEYWORD;


public class AnalyzeFunctionCall implements FormAnalyzer {

  @Override
  public void analyze(LispList form) {
    SyntaxAnalyzer.INSTANCE.analyzeForms(form.getSexpList(), 1);
  }
}
