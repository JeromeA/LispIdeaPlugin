package org.ax1.lisp.analysis;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.Annotator;
import com.intellij.psi.PsiElement;
import org.ax1.lisp.analysis.symbol.SymbolBinding;
import org.ax1.lisp.analysis.symbol.SymbolManager;
import org.ax1.lisp.psi.LispFile;
import org.jetbrains.annotations.NotNull;

import static org.ax1.lisp.analysis.symbol.SymbolBinding.SymbolType.FUNCTION;

public class LispAnnotator implements Annotator {

  public static Annotate EMPTY_ANNOTATE = new Annotate(null, null);

  @Override
  public void annotate(@NotNull PsiElement element, @NotNull AnnotationHolder holder) {
    if (!(element instanceof LispFile)) return;
    LispFile lispFile = (LispFile) element;
    ProjectAnalyser projectAnalyser = ProjectAnalyser.getInstance(lispFile.getProject());

    Annotate annotate = new Annotate(lispFile, holder);
    PackageAnalyzer packageAnalyzer = new PackageAnalyzer(lispFile, annotate);
    packageAnalyzer.analyzePackages();
    SyntaxAnalyzer analyzer = new SyntaxAnalyzer(lispFile, annotate, new SymbolManager(projectAnalyser.getPackages()));
    analyzer.analyze();

    analyzer.lexicalBindings.getRetired()
        .forEach(binding -> checkBinding(binding, annotate));
    projectAnalyser.getSymbolManager().getBindings().forEach(binding -> checkBinding(binding, annotate));
  }

  private void checkBinding(SymbolBinding binding, Annotate annotations) {
    checkNoUsages(binding, annotations);
    checkNoDefinition(binding, annotations);
  }

  private void checkNoDefinition(SymbolBinding symbolBinding, Annotate annotations) {
    if (!symbolBinding.isDefined()) {
      String message = symbolBinding.getSymbolType() == FUNCTION ? "Function '%s' does not exist" : "Variable '%s' is not defined";
      symbolBinding.getUsages().forEach(usage ->
          annotations.highlightUnknown(usage, String.format(message, symbolBinding.getSymbol())));
    }
  }

  private void checkNoUsages(SymbolBinding symbolBinding, Annotate annotations) {
    if (symbolBinding.getUsages().isEmpty() && symbolBinding.getDefinition() != null) {
      String message = symbolBinding.getSymbolType() == FUNCTION ? "Function '%s' is never called" : "Variable '%s' is never used";
      annotations.highlightUnused(symbolBinding.getDefinition(), String.format(message, symbolBinding.getSymbol()));
    }
  }
}
