package org.ax1.lisp.analysis;

import org.ax1.lisp.analysis.symbol.SymbolManager;
import org.ax1.lisp.psi.*;

import java.util.List;

public class PackageAnalyzer {

  private final LispFile lispFile;
  private final AnalyzeDefpackage analyzeDefpackage;
  public final SyntaxAnalyzer analyzer;

  public PackageAnalyzer(LispFile lispFile, Annotate annotate) {
    this.lispFile = lispFile;
    this.analyzeDefpackage = new AnalyzeDefpackage();
    this.analyzer = new SyntaxAnalyzer(lispFile, annotate, new SymbolManager());
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
      analyzeDefpackage.analyze(analyzer, form);
    }
  }
}
