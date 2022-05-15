package org.ax1.lisp.analysis;

import com.intellij.psi.util.CachedValueProvider;
import org.ax1.lisp.analysis.symbol.SymbolManager;
import org.ax1.lisp.psi.LispFile;
import org.jetbrains.annotations.Nullable;

import static org.ax1.lisp.analysis.LispAnnotator.EMPTY_ANNOTATE;

class FileSymbolAnalyzer implements CachedValueProvider<SymbolManager> {

  private final ProjectAnalyser projectAnalyser;
  private final LispFile lispFile;

  public FileSymbolAnalyzer(LispFile lispFile) {
    this.projectAnalyser = ProjectAnalyser.getInstance(lispFile.getProject());
    this.lispFile = lispFile;
  }

  @Override
  public @Nullable Result<SymbolManager> compute() {
    SyntaxAnalyzer analyzer = new SyntaxAnalyzer(lispFile, EMPTY_ANNOTATE, new SymbolManager(projectAnalyser.getPackages()));
    analyzer.analyze();
    if (!analyzer.lexicalBindings.isEmpty()) throw new RuntimeException("Unbalanced lexical stack.");
    return new Result<>(analyzer.symbolManager, lispFile);
  }
}
