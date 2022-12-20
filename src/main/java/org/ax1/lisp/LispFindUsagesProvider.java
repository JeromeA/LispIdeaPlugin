package org.ax1.lisp;

import com.intellij.lang.cacheBuilder.DefaultWordsScanner;
import com.intellij.lang.cacheBuilder.WordsScanner;
import com.intellij.lang.findUsages.FindUsagesProvider;
import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.TokenSet;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.ax1.lisp.parsing.LispLexerAdapter;
import org.ax1.lisp.psi.LispSymbolName;
import org.ax1.lisp.psi.impl.LispStringDesignator;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import static com.intellij.lang.HelpID.FIND_OTHER_USAGES;
import static org.ax1.lisp.psi.LispTypes.*;

public class LispFindUsagesProvider implements FindUsagesProvider {

  @Override
  public boolean canFindUsagesFor(@NotNull PsiElement psiElement) {
    return psiElement instanceof LispStringDesignator;
  }

  @Override
  public @Nullable @NonNls String getHelpId(@NotNull PsiElement psiElement) {
    return FIND_OTHER_USAGES;
  }

  @Override
  public @Nls @NotNull String getType(@NotNull PsiElement element) {
    if (element instanceof LispSymbolName) {
      LispSymbolName symbolName = (LispSymbolName) element;
      SymbolDefinition symbolDefinition = symbolName.getSymbolDefinition();
      if (symbolDefinition == null) return "unknown";
      switch (symbolDefinition.type) {
        case FUNCTION:
          return "Function";
        case VARIABLE:
          return "Variable";
      }
    }
    return "unknown";
  }

  @Override
  public @Nls @NotNull String getDescriptiveName(@NotNull PsiElement element) {
    // TODO: return the whole signature for functions.
    if (element instanceof LispSymbolName) {
      LispSymbolName symbolName = (LispSymbolName) element;
      return "descriptiveName:" + symbolName.getText();
    }
    return "unknown descriptive name";
  }

  @Override
  public @Nls @NotNull String getNodeText(@NotNull PsiElement element, boolean useFullName) {
    return element.getText();
  }

  @Override
  public @Nullable WordsScanner getWordsScanner() {
    return new DefaultWordsScanner(new LispLexerAdapter(),
        TokenSet.create(SYMBOL),
        TokenSet.create(COMMENT),
        TokenSet.create(CHARACTER, NUMBER, STRING));
  }
}
