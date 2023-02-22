package org.ax1.lisp.analysis;

import com.intellij.psi.util.CachedValueProvider;
import org.ax1.lisp.psi.LispFile;
import org.jetbrains.annotations.Nullable;

import static org.ax1.lisp.analysis.LispAnnotator.EMPTY_HIGHLIGHTER;

public class FilePackageAnalyzer implements CachedValueProvider<Bindings> {

  private final LispFile lispFile;

  public FilePackageAnalyzer(LispFile lispFile) {
    this.lispFile = lispFile;
  }

  @Override
  public @Nullable Result<Bindings> compute() {
    PackageAnalyzer packageAnalyzer = new PackageAnalyzer(lispFile, EMPTY_HIGHLIGHTER);
    packageAnalyzer.analyzePackages();
    return new Result<>(packageAnalyzer.context.result, lispFile);
  }
}
