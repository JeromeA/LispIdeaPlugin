package org.ax1.lisp;

import com.intellij.codeInsight.completion.*;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.patterns.PlatformPatterns;
import com.intellij.patterns.StandardPatterns;
import com.intellij.psi.PsiElement;
import com.intellij.util.ProcessingContext;
import org.ax1.lisp.analysis.ProjectData;
import org.ax1.lisp.psi.impl.LispStringDesignator;
import org.jetbrains.annotations.NotNull;

import static com.intellij.codeInsight.completion.CompletionUtilCore.DUMMY_IDENTIFIER_TRIMMED;
import static org.ax1.lisp.psi.LispTypes.STRING_CONTENT_TOKEN;
import static org.ax1.lisp.psi.LispTypes.SYMBOL_TOKEN;
import static org.ax1.lisp.psi.impl.LispStringDesignator.Type.FUNCTION_USAGE;
import static org.ax1.lisp.psi.impl.LispStringDesignator.Type.VARIABLE_USAGE;

public class LispCompletionContributor extends CompletionContributor {

  public LispCompletionContributor() {
    extend(CompletionType.BASIC, StandardPatterns.or(PlatformPatterns.psiElement(SYMBOL_TOKEN), PlatformPatterns.psiElement(STRING_CONTENT_TOKEN)), new LispCompletionProvider());
  }

  private static class LispCompletionProvider extends CompletionProvider<CompletionParameters> {
    @Override
    protected void addCompletions(@NotNull CompletionParameters parameters, @NotNull ProcessingContext context, @NotNull CompletionResultSet result) {
      PsiElement symbolToken = parameters.getPosition();
      String tokenText = symbolToken.getText();
      result = result.withPrefixMatcher(tokenText.substring(0, tokenText.length() - DUMMY_IDENTIFIER_TRIMMED.length()));
      // If we are data, no completion.
      if (!(symbolToken instanceof LispStringDesignator)) {
        return;
      }
      LispStringDesignator stringDesignator = (LispStringDesignator) symbolToken;
      // If we are a function usage, complete with all known functions
      if (stringDesignator.getType() == FUNCTION_USAGE) {
        ProjectData.getInstance(symbolToken.getProject())
            .getAllFunctionDefinitionNames()
            .stream()
            .map(LookupElementBuilder::create)
            .forEach(result::addElement);
      }
      // If we are a variable usae, complete with all global variables
      if (stringDesignator.getType() == VARIABLE_USAGE) {
        ProjectData.getInstance(symbolToken.getProject())
            .getAllVariableDefinitionNames()
            .stream()
            .map(LookupElementBuilder::create)
            .forEach(result::addElement);
      }
    }
  }
}
