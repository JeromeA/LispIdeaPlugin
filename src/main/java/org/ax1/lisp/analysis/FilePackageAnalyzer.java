package org.ax1.lisp.analysis;

import com.intellij.psi.util.CachedValueProvider;
import org.ax1.lisp.analysis.symbol.PackageDefinition;
import org.ax1.lisp.psi.LispFile;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;

import static org.ax1.lisp.analysis.LispAnnotator.EMPTY_ANNOTATE;

public class FilePackageAnalyzer implements CachedValueProvider<Collection<PackageDefinition>> {

  private final LispFile lispFile;

  public FilePackageAnalyzer(LispFile lispFile) {
    this.lispFile = lispFile;
  }

  @Override
  public @Nullable Result<Collection<PackageDefinition>> compute() {
    PackageAnalyzer packageAnalyzer = new PackageAnalyzer(lispFile, EMPTY_ANNOTATE);
    packageAnalyzer.analyzePackages();
    return new Result<>(packageAnalyzer.analyzer.packages, lispFile);
  }
}
