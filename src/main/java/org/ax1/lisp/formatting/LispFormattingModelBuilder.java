package org.ax1.lisp.formatting;

import com.intellij.formatting.*;
import com.intellij.psi.codeStyle.CodeStyleSettings;
import org.jetbrains.annotations.NotNull;

public class LispFormattingModelBuilder implements FormattingModelBuilder {

  @Override
  public @NotNull FormattingModel createModel(@NotNull FormattingContext formattingContext) {
    final CodeStyleSettings codeStyleSettings = formattingContext.getCodeStyleSettings();
    return FormattingModelProvider.createFormattingModelForPsiFile(
        formattingContext.getContainingFile(),
        new LispBlock(formattingContext.getNode(), null),
        codeStyleSettings);
  }
}
