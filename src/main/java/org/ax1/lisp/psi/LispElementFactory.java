package org.ax1.lisp.psi;

import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiFileFactory;
import org.ax1.lisp.LispFileType;

public class LispElementFactory {
  public static LispList createCons(Project project, String name) {
    final LispFile file = createFile(project, "(defun " + name + " ())");
    // The first child is a sexp, whose first child is a cons.
    return (LispList) file.getFirstChild().getFirstChild();
  }

  public static LispSymbol createSymbol(Project project, String name) {
    final LispFile file = createFile(project, name);
    // The first child is a sexp, whose first child is a symbol.
    return (LispSymbol) file.getFirstChild().getFirstChild();
  }

  public static LispFile createFile(Project project, String text) {
    String name = "dummy.lisp";
    return (LispFile) PsiFileFactory.getInstance(project)
        .createFileFromText(name, LispFileType.INSTANCE, text);
  }

}
