package org.ax1.lisp.psi;

import com.intellij.extapi.psi.PsiFileBase;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.psi.FileViewProvider;
import org.ax1.lisp.LispFileType;
import org.ax1.lisp.LispLanguage;
import org.jetbrains.annotations.NotNull;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import static org.ax1.lisp.psi.impl.LispListMixin.ListSyntaxType.FUNCTION_CALL;
import static org.ax1.lisp.psi.impl.LispSymbolMixin.SymbolSyntaxType.VARIABLE_USAGE;

public class LispFile extends PsiFileBase {

  public LispFile(@NotNull FileViewProvider viewProvider) {
    super(viewProvider, LispLanguage.INSTANCE);
  }

  @NotNull
  @Override
  public FileType getFileType() {
    return LispFileType.INSTANCE;
  }

  @Override
  public String toString() {
    return "Lisp File";
  }

  public List<LispSexp> getSexpList() {
    return Arrays.stream(getChildren()).filter(psi -> psi instanceof LispSexp).map(psi -> (LispSexp)psi).collect(Collectors.toList());
  }

  public void computeSyntaxType() {
    for (LispSexp sexp : getSexpList()) {
      LispSymbol symbol = sexp.getSymbol();
      if (symbol != null) {
        symbol.setSyntaxType(VARIABLE_USAGE);
      }
      LispList list = sexp.getList();
      if (list != null) {
        list.setSyntaxType(FUNCTION_CALL);
      }
    }
  }
}
