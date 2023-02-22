package org.ax1.lisp.analysis;

import com.intellij.lang.annotation.AnnotationHolder;
import org.ax1.lisp.analysis.symbol.Lambda;
import org.ax1.lisp.analysis.symbol.PackageManager;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.ax1.lisp.psi.*;
import org.ax1.lisp.psi.impl.LispStringDesignator;
import org.jetbrains.annotations.NotNull;

import static org.ax1.lisp.analysis.symbol.SymbolDefinition.Type.FUNCTION;

public class LispAnnotator {

  public static final Highlighter EMPTY_HIGHLIGHTER = new Highlighter(null, null);

  private final LispFile lispFile;
  private final Highlighter highlighter;

  public LispAnnotator(@NotNull LispFile lispFile, AnnotationHolder holder) {
    this.lispFile = lispFile;
    this.highlighter = new Highlighter(lispFile, holder);
  }

  public void run() {
    ProjectComputedData projectComputedData = ProjectComputedData.getInstance(lispFile.getProject());

    SyntaxAnalyzer analyzer = new SyntaxAnalyzer(lispFile);
    AnalysisContext context = new AnalysisContext(highlighter, new PackageManager(projectComputedData.getPackageDefinitions()), analyzer);
    analyzer.setContext(context);
    analyzer.analyze();

    context.lexicalBindings.getRetiredBindings().forEach(this::checkNoUsages);
    ProjectDefinitions projectAnalysis = projectComputedData.getProjectAnalysis();
    projectAnalysis.getFunctions().forEach(this::checkSymbol);
    projectAnalysis.getVariables().forEach(this::checkSymbol);
  }

  private void checkSymbol(SymbolDefinition symbolDefinition) {
    checkNoUsages(symbolDefinition);
    checkNoDefinition(symbolDefinition);
    checkArguments(symbolDefinition);
  }

  private void checkNoDefinition(SymbolDefinition symbolDefinition) {
    if (!symbolDefinition.isDefined()) {
      String message = symbolDefinition.type == FUNCTION ? "Function '%s' does not exist" : "Variable '%s' is not defined";
      symbolDefinition.getUsages().forEach(usage ->
          highlighter.highlightUnknown(usage, String.format(message, symbolDefinition.symbol)));
    }
  }

  private void checkNoUsages(SymbolDefinition symbolDefinition) {
    if (symbolDefinition.getUsages().isEmpty() && !symbolDefinition.getDefinitions().isEmpty()) {
      String message = symbolDefinition.type == FUNCTION ? "Function '%s' is never called" : "Variable '%s' is never used";
      highlighter.highlightUnused(symbolDefinition.getDefinition(), String.format(message, symbolDefinition.symbol));
    }
  }

  private void checkArguments(SymbolDefinition symbolDefinition) {
    Lambda lambda = symbolDefinition.getLambda();
    if (lambda == null) return;
    ArgumentChecker checker = new ArgumentChecker(symbolDefinition.symbol, lambda, highlighter);
    symbolDefinition.getUsages().stream().filter(this::localToFile).forEach(checker::checkArguments);
  }

  private boolean localToFile(LispStringDesignator lispStringDesignator) {
    return lispStringDesignator.getContainingFile() == lispFile;
  }
}
