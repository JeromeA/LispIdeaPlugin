package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalyzerContext;
import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.psi.LispList;


public class AnalyzeFunctionCall implements FormAnalyzer {

  @Override
  public void analyze(AnalyzerContext context, LispList form) {
    SyntaxAnalyzer.INSTANCE.analyzeForms(context, form.getSexpList(), 1);
  }
}
