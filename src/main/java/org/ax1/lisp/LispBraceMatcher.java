package org.ax1.lisp;

import com.intellij.codeInsight.highlighting.PairedBraceMatcherAdapter;
import com.intellij.lang.BracePair;
import com.intellij.lang.PairedBraceMatcher;
import com.intellij.psi.PsiFile;
import com.intellij.psi.tree.IElementType;
import org.ax1.lisp.psi.LispTypes;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class LispBraceMatcher extends PairedBraceMatcherAdapter {

  public LispBraceMatcher() {
    super(new LispPairedBraceMatcher(), LispLanguage.INSTANCE);
  }

  static class LispPairedBraceMatcher implements PairedBraceMatcher {

    @Override
    public BracePair @NotNull [] getPairs() {
      return new BracePair[] {
          new BracePair(LispTypes.LPAREN, LispTypes.RPAREN, true),
      };
    }

    @Override
    public boolean isPairedBracesAllowedBeforeType(@NotNull IElementType lbraceType, @Nullable IElementType contextType) {
      return true;
    }

    @Override
    public int getCodeConstructStart(PsiFile file, int openingBraceOffset) {
      return openingBraceOffset;
    }
  }
}
