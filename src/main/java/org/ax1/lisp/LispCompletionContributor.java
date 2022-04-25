package org.ax1.lisp;

import com.intellij.codeInsight.completion.*;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.patterns.PlatformPatterns;
import com.intellij.psi.PsiElement;
import com.intellij.util.ProcessingContext;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.analysis.symbol.SymbolManager;
import org.ax1.lisp.psi.LispFile;
import org.ax1.lisp.psi.LispList;
import org.jetbrains.annotations.NotNull;

import static org.ax1.lisp.psi.LispTypes.SYMBOL_TOKEN;

public class LispCompletionContributor extends CompletionContributor {

  public LispCompletionContributor() {
    extend(CompletionType.BASIC, PlatformPatterns.psiElement(SYMBOL_TOKEN), new CompletionProvider<>() {
      @Override
      protected void addCompletions(@NotNull CompletionParameters parameters, @NotNull ProcessingContext context, @NotNull CompletionResultSet result) {
        SymbolManager symbolManager = LispProject.getInstance(parameters.getPosition().getProject()).getSymbolManager();
        if (isFunctionPosition(parameters)) {
          addFunctionCompletions(symbolManager, result);
        } else {
          addVariableCompletions(symbolManager, result);
        }
      }
    });
  }

  private boolean isFunctionPosition(@NotNull CompletionParameters parameters) {
    PsiElement newSymbolToken = parameters.getPosition();
    PsiElement newSymbol = newSymbolToken.getParent();
    PsiElement sexp = newSymbol.getParent();
    PsiElement container = sexp.getParent();
    if (container instanceof LispFile) return false;
    return ((LispList) container).getSexpList().get(0) == sexp;
  }

  private void addFunctionCompletions(SymbolManager symbolManager, CompletionResultSet result) {
    symbolManager.getAvailableFunctions().stream()
        .map(Symbol::getName)
        .map(String::toLowerCase)
        .forEach(name -> result.addElement(LookupElementBuilder.create(name)));
  }

  private void addVariableCompletions(SymbolManager symbolManager, CompletionResultSet result) {
    symbolManager.getAvailableVariables().stream()
        .map(Symbol::getName)
        .map(String::toLowerCase)
        .forEach(name -> result.addElement(LookupElementBuilder.create(name)));
  }
}
