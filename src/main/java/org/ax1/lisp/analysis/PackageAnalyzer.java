package org.ax1.lisp.analysis;

import org.ax1.lisp.analysis.form.AnalyzeDefpackage;
import org.ax1.lisp.analysis.symbol.PackageManager;
import org.ax1.lisp.psi.*;

import java.util.List;

public class PackageAnalyzer {

  private final LispFile lispFile;
  private final AnalyzeDefpackage analyzeDefpackage;
  public final AnalysisContext context;

  public PackageAnalyzer(LispFile lispFile, Highlighter highlighter) {
    this.lispFile = lispFile;
    this.analyzeDefpackage = new AnalyzeDefpackage();
    this.context = new AnalysisContext(highlighter, new PackageManager(), null);
  }

  public void analyzePackages() {
    lispFile.getSexpList().forEach(this::checkDefpackage);
  }

  private void checkDefpackage(LispSexp sexp) {
    LispList form = sexp.getList();
    if (form == null) return;
    List<LispSexp> sexpList = form.getSexpList();
    if (sexpList.size() < 1) return;
    LispSymbol symbol = sexpList.get(0).getSymbol();
    if (symbol == null) return;
    if (symbol.getText().equals("defpackage")) {
      analyzeDefpackage.analyze(context, form);
    }
  }
}
