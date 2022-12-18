package org.ax1.lisp.refactoring;

import com.intellij.patterns.ElementPattern;
import com.intellij.patterns.PlatformPatterns;
import com.intellij.psi.PsiElement;
import com.intellij.refactoring.rename.RenameInputValidator;
import com.intellij.util.ProcessingContext;
import org.ax1.lisp.psi.LispSexp;
import org.jetbrains.annotations.NotNull;

import static org.ax1.lisp.refactoring.LispNamesValidator.SYMBOL_PATTERN;

public class LispRenameInputValidator implements RenameInputValidator {

  @Override
  public @NotNull ElementPattern<? extends PsiElement> getPattern() {
    return PlatformPatterns.psiElement(LispSexp.class);
  }

  @Override
  public boolean isInputValid(@NotNull String newName, @NotNull PsiElement element, @NotNull ProcessingContext context) {
    return SYMBOL_PATTERN.matcher(newName).matches();
  }
}
