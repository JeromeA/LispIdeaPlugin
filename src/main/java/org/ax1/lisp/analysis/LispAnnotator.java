package org.ax1.lisp.analysis;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.Annotator;
import com.intellij.psi.PsiElement;
import org.ax1.lisp.LispProject;
import org.ax1.lisp.analysis.symbol.SymbolBinding;
import org.ax1.lisp.analysis.symbol.SymbolManager;
import org.ax1.lisp.psi.LispFile;
import org.jetbrains.annotations.NotNull;

import static org.ax1.lisp.analysis.symbol.SymbolBinding.SymbolType.FUNCTION;

public class LispAnnotator implements Annotator {

  @Override
  public void annotate(@NotNull PsiElement element, @NotNull AnnotationHolder holder) {
    if (!(element instanceof LispFile)) return;
    LispFile lispFile = (LispFile) element;
    LispProject lispProject = LispProject.getInstance(lispFile.getProject());

    // Re-run package and symbol analysis for this file, so that we can annotate syntax, and recover lexical bindings.
    Annotate annotate = new Annotate(lispFile, holder);
    lispProject.updatePackagesForFile(lispFile, annotate);
    SyntaxAnalyzer syntaxAnalyzer = lispProject.updateSymbolsForFile(lispFile, annotate);
    if (!syntaxAnalyzer.lexicalBindings.isEmpty()) throw new RuntimeException("Unbalanced lexical stack.");

    // Use the end result to annotate the file.
    annotate(lispProject, syntaxAnalyzer);
  }

  private void annotate(LispProject lispProject, SyntaxAnalyzer syntaxAnalyzer) {
    syntaxAnalyzer.lexicalBindings.getRetired().forEach(binding -> checkBinding(syntaxAnalyzer, binding));
    SymbolManager symbolManager = lispProject.getSymbolManager();
    symbolManager.getBindings().forEach(binding -> checkBinding(syntaxAnalyzer, binding));
  }

  private void checkBinding(SyntaxAnalyzer syntaxAnalyzer, SymbolBinding binding) {
    checkNoUsages(syntaxAnalyzer, binding);
    checkNoDefinition(syntaxAnalyzer, binding);
  }

  private void checkNoDefinition(SyntaxAnalyzer syntaxAnalyzer, SymbolBinding symbolBinding) {
    if (!symbolBinding.isDefined()) {
      String message = symbolBinding.getSymbolType() == FUNCTION ? "Function '%s' does not exist" : "Variable '%s' is not defined";
      symbolBinding.getUsages().forEach(usage ->
          syntaxAnalyzer.annotations.highlightUnknown(usage, String.format(message, symbolBinding.getSymbol())));
    }
  }

  private void checkNoUsages(SyntaxAnalyzer syntaxAnalyzer, SymbolBinding symbolBinding) {
    if (symbolBinding.getUsages().isEmpty() && symbolBinding.getDefinition() != null) {
      String message = symbolBinding.getSymbolType() == FUNCTION ? "Function '%s' is never called" : "Variable '%s' is never used";
      syntaxAnalyzer.annotations.highlightUnused(symbolBinding.getDefinition(), String.format(message, symbolBinding.getSymbol()));
    }
  }
}
