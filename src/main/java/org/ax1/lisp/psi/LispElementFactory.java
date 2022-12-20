package org.ax1.lisp.psi;

import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiFileFactory;
import org.ax1.lisp.LispFileType;

import javax.swing.text.StringContent;

public class LispElementFactory {

  public static LispSymbolName createSymbolName(Project project, String name) {
    final LispFile file = createFile(project, name);
    // The first child is a sexp, whose first child is a symbol
    LispSexp sexp = (LispSexp) file.getFirstChild();
    return sexp.getSymbol().getSymbolName();
  }

  public static LispStringContent createStringContent(Project project, String content) {
    final LispFile file = createFile(project, String.format("\"%s\"", content));
    // The first child is a sexp, whose first child is a string, whose second child is a StringContent.
    LispSexp sexp = (LispSexp) file.getFirstChild();
    return sexp.getString().getStringContent();
  }

  public static LispFile createFile(Project project, String text) {
    String name = "dummy.lisp";
    return (LispFile) PsiFileFactory.getInstance(project)
        .createFileFromText(name, LispFileType.INSTANCE, text);
  }

}
