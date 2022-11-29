package org.ax1.lisp.analysis;

import com.intellij.psi.util.CachedValueProvider;
import org.ax1.lisp.analysis.symbol.PackageManager;
import org.ax1.lisp.psi.LispFile;
import org.jetbrains.annotations.Nullable;

import static org.ax1.lisp.analysis.LispAnnotator.EMPTY_HIGHLIGHTER;

class FileAnalysisProvider implements CachedValueProvider<Bindings> {

  private final ProjectComputedData projectComputedData;
  private final LispFile lispFile;

  public FileAnalysisProvider(LispFile lispFile) {
    this.projectComputedData = ProjectComputedData.getInstance(lispFile.getProject());
    this.lispFile = lispFile;
  }

  @Override
  public @Nullable Result<Bindings> compute() {
    SyntaxAnalyzer analyzer = new SyntaxAnalyzer(lispFile);
    AnalysisContext context = new AnalysisContext(EMPTY_HIGHLIGHTER, new PackageManager(projectComputedData.getPackages()), analyzer);
    analyzer.setContext(context);
    analyzer.analyze();
    if (!context.lexicalBindings.isEmpty()) throw new RuntimeException("Unbalanced lexical stack.");
    return new Result<>(context.result, lispFile);
  }
}
