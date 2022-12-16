package org.ax1.lisp.psi;

import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiFileFactory;
import org.ax1.lisp.LispFileType;

public class LispElementFactory {

  public static LispSexp createSymbol(Project project, String name) {
    final LispFile file = createFile(project, name);
    // The first child is a sexp, whose first child is a symbol.
    return (LispSexp) file.getFirstChild();
  }

  public static LispFile createFile(Project project, String text) {
    String name = "dummy.lisp";
    return (LispFile) PsiFileFactory.getInstance(project)
        .createFileFromText(name, LispFileType.INSTANCE, text);
  }

}
