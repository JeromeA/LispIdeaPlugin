package org.ax1.lisp.analysis;

import com.intellij.psi.util.CachedValueProvider;
import org.ax1.lisp.analysis.symbol.PackageManager;
import org.ax1.lisp.psi.LispFile;
import org.jetbrains.annotations.Nullable;

import static org.ax1.lisp.analysis.LispAnnotator.EMPTY_ANNOTATE;

class FileSymbolAnalyzer implements CachedValueProvider<FileSymbolAnalysis> {

  private final ProjectAnalyser projectAnalyser;
  private final LispFile lispFile;

  public FileSymbolAnalyzer(LispFile lispFile) {
    this.projectAnalyser = ProjectAnalyser.getInstance(lispFile.getProject());
    this.lispFile = lispFile;
  }

  @Override
  public @Nullable Result<FileSymbolAnalysis> compute() {
    SyntaxAnalyzer analyzer = new SyntaxAnalyzer(lispFile, EMPTY_ANNOTATE, new PackageManager(projectAnalyser.getPackages()));
    analyzer.analyze();
    if (!analyzer.lexicalBindings.isEmpty()) throw new RuntimeException("Unbalanced lexical stack.");
    FileSymbolAnalysis result = new FileSymbolAnalysis();
    result.functions = analyzer.packageManager.getFunctions();
    result.variables = analyzer.packageManager.getVariables();
    result.retiredBindings = analyzer.lexicalBindings.getRetired();
    return new Result<>(result, lispFile);
  }
}
