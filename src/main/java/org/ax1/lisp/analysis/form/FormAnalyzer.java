package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.psi.LispList;

public interface FormAnalyzer {
  void analyze(SyntaxAnalyzer analyzer, LispList form);
}
