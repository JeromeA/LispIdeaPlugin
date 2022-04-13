package org.ax1.lisp.analysis;

import org.ax1.lisp.psi.LispList;

public interface Analyzer {
  void analyze(SyntaxAnalyzer analyzer, LispList form);
}
