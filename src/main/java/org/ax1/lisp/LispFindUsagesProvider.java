package org.ax1.lisp;

import com.intellij.lang.cacheBuilder.DefaultWordsScanner;
import com.intellij.lang.cacheBuilder.WordsScanner;
import com.intellij.lang.findUsages.FindUsagesProvider;
import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.TokenSet;
import org.ax1.lisp.analysis.Strings;
import org.ax1.lisp.analysis.SymbolBinding;
import org.ax1.lisp.analysis.symbol.PackageDefinition;
import org.ax1.lisp.parsing.LispLexerAdapter;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import static com.intellij.lang.HelpID.FIND_OTHER_USAGES;
import static org.ax1.lisp.psi.LispTypes.*;

public class LispFindUsagesProvider implements FindUsagesProvider {

  @Override
  public boolean canFindUsagesFor(@NotNull PsiElement psiElement) {
    return psiElement instanceof LispSymbol || psiElement instanceof LispSexp;
  }

  @Override
  public @Nullable @NonNls String getHelpId(@NotNull PsiElement psiElement) {
    return FIND_OTHER_USAGES;
  }

  @Override
  public @Nls @NotNull String getType(@NotNull PsiElement element) {
    // TODO: fix this. It doesn't work, probably because FindUsages works on a new copy of PSI tree, which does not
    // contain the analysis results.
    if (element instanceof LispSymbol) {
      LispSymbol lispSymbol = (LispSymbol) element;
      SymbolBinding symbolDefinition = lispSymbol.getSymbolDefinition();
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
    if (element instanceof LispSymbol) {
      LispSymbol lispSymbol = (LispSymbol) element;
      return "descriptiveName:" + lispSymbol.getText();
    }
    return "unknown descriptive name";
  }

  @Override
  public @Nls @NotNull String getNodeText(@NotNull PsiElement element, boolean useFullName) {
    if (element instanceof LispSymbol) {
      LispSymbol symbol = (LispSymbol) element;
      SymbolBinding symbolDefinition = symbol.getSymbolDefinition();
      if (symbolDefinition != null) {
        return symbolDefinition.getName();
      }
    }
    if (element instanceof LispSexp) {
      LispSexp sexp = (LispSexp) element;
      PackageDefinition packageDefinition = sexp.getPackageDefinition();
      if (packageDefinition != null) {
        return packageDefinition.getName();
      }
    }
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
