package org.ax1.lisp.refactoring;

import com.intellij.patterns.ElementPattern;
import com.intellij.patterns.PlatformPatterns;
import com.intellij.patterns.StandardPatterns;
import com.intellij.psi.PsiElement;
import com.intellij.refactoring.rename.RenameInputValidator;
import com.intellij.util.ProcessingContext;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSymbol;
import org.jetbrains.annotations.NotNull;

import static org.ax1.lisp.refactoring.LispNamesValidator.SYMBOL_PATTERN;

public class LispRenameInputValidator implements RenameInputValidator {

  @Override
  public @NotNull ElementPattern<? extends PsiElement> getPattern() {
    return StandardPatterns.or(
        PlatformPatterns.psiElement(LispList.class),
        PlatformPatterns.psiElement(LispSymbol.class));
  }

  @Override
  public boolean isInputValid(@NotNull String newName, @NotNull PsiElement element, @NotNull ProcessingContext context) {
    return SYMBOL_PATTERN.matcher(newName).matches();
  }
}
