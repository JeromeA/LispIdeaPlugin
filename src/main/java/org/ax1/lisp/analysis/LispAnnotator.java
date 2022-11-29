package org.ax1.lisp.analysis;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.Annotator;
import com.intellij.psi.PsiElement;
import org.ax1.lisp.analysis.symbol.PackageManager;
import org.ax1.lisp.psi.LispFile;
import org.jetbrains.annotations.NotNull;

import static org.ax1.lisp.analysis.SymbolBinding.Type.FUNCTION;

public class LispAnnotator implements Annotator {

  public static final Highlighter EMPTY_HIGHLIGHTER = new Highlighter(null, null);

  @Override
  public void annotate(@NotNull PsiElement element, @NotNull AnnotationHolder holder) {
    if (!(element instanceof LispFile)) return;
    LispFile lispFile = (LispFile) element;
    ProjectComputedData projectComputedData = ProjectComputedData.getInstance(lispFile.getProject());

    Highlighter highlighter = new Highlighter(lispFile, holder);
    SyntaxAnalyzer analyzer = new SyntaxAnalyzer(lispFile);
    AnalysisContext context = new AnalysisContext(highlighter, new PackageManager(projectComputedData.getPackages()), analyzer);
    analyzer.setContext(context);
    analyzer.analyze();

    context.lexicalBindings.getRetiredFunctions().forEach(lexicalSymbol -> checkNoUsages(lexicalSymbol, highlighter));
    ProjectDefinitions projectAnalysis = projectComputedData.getProjectAnalysis();
    projectAnalysis.getFunctions().forEach(binding -> checkBinding(binding, highlighter));
    projectAnalysis.getVariables().forEach(binding -> checkBinding(binding, highlighter));
  }

  private void checkBinding(SymbolBinding binding, Highlighter annotations) {
    checkNoUsages(binding, annotations);
    checkNoDefinition(binding, annotations);
  }

  private void checkNoDefinition(SymbolBinding symbolBinding, Highlighter annotations) {
    if (symbolBinding.definitions.isEmpty() && symbolBinding.description == null) {
      String message = symbolBinding.type == FUNCTION ? "Function '%s' does not exist" : "Variable '%s' is not defined";
      symbolBinding.usages.forEach(usage ->
          annotations.highlightUnknown(usage, String.format(message, symbolBinding.symbol)));
    }
  }

  private void checkNoUsages(SymbolBinding symbolBinding, Highlighter annotations) {
    if (symbolBinding.usages.isEmpty() && !symbolBinding.definitions.isEmpty()) {
      String message = symbolBinding.type == FUNCTION ? "Function '%s' is never called" : "Variable '%s' is never used";
      annotations.highlightUnused(symbolBinding.definitions.get(0), String.format(message, symbolBinding.symbol));
    }
  }
}
