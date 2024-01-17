package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalyzerContext;
import org.ax1.lisp.psi.LispList;

public interface FormAnalyzer {
  void analyze(AnalyzerContext context, LispList form);
}
