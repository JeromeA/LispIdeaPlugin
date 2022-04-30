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
    if (element instanceof LispFile){
      LispFile lispFile = (LispFile) element;
      Annotate annotate = new Annotate(lispFile, holder);
      LispProject lispProject = LispProject.getInstance(lispFile.getProject());
      PackageAnalyzer packageAnalyzer = new PackageAnalyzer(lispFile, annotate);
      packageAnalyzer.analyzePackages();
      lispProject.setPackages(lispFile, packageAnalyzer.analyzer.symbolManager.getUserDefinedPackages());
      SyntaxAnalyzer syntaxAnalyzer = new SyntaxAnalyzer(lispFile, new Annotate(lispFile, holder), new SymbolManager(lispProject.getPackages()));
      syntaxAnalyzer.analyze();
      if (!syntaxAnalyzer.lexicalBindings.isEmpty()) throw new RuntimeException("Unbalanced lexical stack.");
      syntaxAnalyzer.lexicalBindings.getRetired().forEach(binding -> checkBinding(syntaxAnalyzer, binding));
      SymbolManager symbolManager = lispProject.getSymbolManager();
      symbolManager.getFunctions().forEach(binding -> checkBinding(syntaxAnalyzer, binding));
      symbolManager.getVariables().forEach(binding -> checkBinding(syntaxAnalyzer, binding));
    }
  }

  private void checkBinding(SyntaxAnalyzer syntaxAnalyzer, SymbolBinding binding) {
    checkNoUsages(syntaxAnalyzer, binding);
    checkNoDefinition(syntaxAnalyzer, binding);
  }

  private void checkNoDefinition(SyntaxAnalyzer syntaxAnalyzer, SymbolBinding symbolBinding) {
    if (!symbolBinding.isDefined()) {
      String message = symbolBinding.getSymbolType() == FUNCTION ? "Function '%s' does not exist" : "Variable '%s' is not defined";
      symbolBinding.getUsages().forEach(usage ->
          syntaxAnalyzer.annotations.highlightUnknown(usage, String.format(message, symbolBinding.getName())));
    }
  }

  private void checkNoUsages(SyntaxAnalyzer syntaxAnalyzer, SymbolBinding symbolBinding) {
    if (symbolBinding.getUsages().isEmpty() && symbolBinding.getDefinition() != null) {
      String message = symbolBinding.getSymbolType() == FUNCTION ? "Function '%s' is never called" : "Variable '%s' is never used";
      syntaxAnalyzer.annotations.highlightUnused(symbolBinding.getDefinition(), String.format(message, symbolBinding.getName()));
    }
  }
}
