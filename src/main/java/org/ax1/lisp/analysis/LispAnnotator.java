package org.ax1.lisp.analysis;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.Annotator;
import com.intellij.psi.PsiElement;
import org.ax1.lisp.analysis.symbol.PackageManager;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.ax1.lisp.psi.LispFile;
import org.jetbrains.annotations.NotNull;

import static org.ax1.lisp.analysis.symbol.SymbolDefinition.Type.FUNCTION;

public class LispAnnotator implements Annotator {

  public static final Highlighter EMPTY_HIGHLIGHTER = new Highlighter(null, null);

  @Override
  public void annotate(@NotNull PsiElement element, @NotNull AnnotationHolder holder) {
    if (!(element instanceof LispFile)) return;
    LispFile lispFile = (LispFile) element;
    ProjectComputedData projectComputedData = ProjectComputedData.getInstance(lispFile.getProject());

    Highlighter highlighter = new Highlighter(lispFile, holder);
    SyntaxAnalyzer analyzer = new SyntaxAnalyzer(lispFile);
    AnalysisContext context = new AnalysisContext(highlighter, new PackageManager(projectComputedData.getPackageDefinitions()), analyzer);
    analyzer.setContext(context);
    analyzer.analyze();

    context.lexicalBindings.getRetiredBindings().forEach(lexicalSymbol -> checkNoUsages(lexicalSymbol, highlighter));
    ProjectDefinitions projectAnalysis = projectComputedData.getProjectAnalysis();
    projectAnalysis.getFunctions().forEach(binding -> checkBinding(binding, highlighter));
    projectAnalysis.getVariables().forEach(binding -> checkBinding(binding, highlighter));
  }

  private void checkBinding(SymbolDefinition binding, Highlighter annotations) {
    checkNoUsages(binding, annotations);
    checkNoDefinition(binding, annotations);
  }

  private void checkNoDefinition(SymbolDefinition symbolDefinition, Highlighter annotations) {
    if (!symbolDefinition.isDefined()) {
      String message = symbolDefinition.type == FUNCTION ? "Function '%s' does not exist" : "Variable '%s' is not defined";
      symbolDefinition.getUsages().forEach(usage ->
          annotations.highlightUnknown(usage, String.format(message, symbolDefinition.symbol)));
    }
  }

  private void checkNoUsages(SymbolDefinition symbolDefinition, Highlighter annotations) {
    if (symbolDefinition.getUsages().isEmpty() && !symbolDefinition.getDefinitions().isEmpty()) {
      String message = symbolDefinition.type == FUNCTION ? "Function '%s' is never called" : "Variable '%s' is never used";
      annotations.highlightUnused(symbolDefinition.getDefinition(), String.format(message, symbolDefinition.symbol));
    }
  }
}
