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

import static org.ax1.lisp.parsing.LispFeatureExpressions.filterOptionalSexpList;

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
    return filterOptionalSexpList(
        Arrays.stream(getChildren())
            .filter(x -> x instanceof LispOptionalSexp)
            .map(x -> (LispOptionalSexp)x)
            .collect(Collectors.toUnmodifiableList()));
  }
}
