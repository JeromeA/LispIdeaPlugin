package org.ax1.lisp;

import com.intellij.codeInsight.completion.*;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.patterns.StandardPatterns;
import com.intellij.psi.PsiElement;
import com.intellij.util.ProcessingContext;
import org.ax1.lisp.analysis.BaseLispElement.Type;
import org.ax1.lisp.analysis.ProjectData;
import org.ax1.lisp.analysis.symbol.CommonLispPackage;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.impl.LispStringDesignator;
import org.jetbrains.annotations.NotNull;

import java.util.*;

import static com.intellij.codeInsight.completion.CompletionUtilCore.DUMMY_IDENTIFIER_TRIMMED;
import static com.intellij.patterns.PlatformPatterns.psiElement;
import static org.ax1.lisp.psi.LispTypes.STRING_CONTENT_TOKEN;
import static org.ax1.lisp.psi.LispTypes.SYMBOL_TOKEN;

public class LispCompletionContributor extends CompletionContributor {

  public LispCompletionContributor() {
    extend(CompletionType.BASIC,
        StandardPatterns.or(psiElement(SYMBOL_TOKEN), psiElement(STRING_CONTENT_TOKEN)),
        new LispCompletionProvider());
  }

  private static class LispCompletionProvider extends CompletionProvider<CompletionParameters> {
    @Override
    protected void addCompletions(@NotNull CompletionParameters parameters, @NotNull ProcessingContext context, @NotNull CompletionResultSet result) {
      PsiElement symbolToken = parameters.getPosition();
      String tokenText = symbolToken.getText();
      String prefix = tokenText.substring(0, tokenText.length() - DUMMY_IDENTIFIER_TRIMMED.length());
      boolean isPrefixUpper = !prefix.isEmpty() && prefix.toUpperCase().equals(prefix);
      result = result.withPrefixMatcher(prefix);
      LispStringDesignator stringDesignator = getStringDesignator(symbolToken);
      if (stringDesignator == null) return;
      ProjectData projectData = ProjectData.getInstance(symbolToken.getProject());
      Type type = stringDesignator.getType();
      Set<String> names = new HashSet<>();
      if (type == Type.FUNCTION_USAGE) {
        names.addAll(projectData.getAllFunctionDefinitionNames());
      }
      if (type == Type.VARIABLE_USAGE) {
        names.addAll(projectData.getAllVariableDefinitionNames());
        names.addAll(getAllLexicalVariables(symbolToken));
      }
      // TODO: lookup relevant packages, instead of just COMMON-LISP.
      // TODO: filter the exported names to keep only the variable or function depending on the case.
      names.addAll(CommonLispPackage.INSTANCE.exports);
      names.stream()
          .map(n -> isPrefixUpper ? n : n.toLowerCase())
          .map(LookupElementBuilder::create)
          .forEach(result::addElement);
    }

    private Collection<String> getAllLexicalVariables(PsiElement element) {
      List<String> result = new ArrayList<>();
      while (element != null) {
        if (element instanceof LispSexp) {
          ((LispSexp) element).getLexicalVariables().keySet().stream()
              .map(Symbol::getName)
              .forEach(result::add);
        }
        element = element.getParent();
      }
      return result;
    }

    private LispStringDesignator getStringDesignator(PsiElement element) {
      while (true) {
        if (element instanceof LispStringDesignator) return (LispStringDesignator) element;
        if (element instanceof LispSexp) return null;
        element = element.getParent();
      }
    }
  }
}
