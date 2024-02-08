package org.ax1.lisp.psi;

import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiFileFactory;
import org.ax1.lisp.LispFileType;

import javax.swing.text.StringContent;

public class LispElementFactory {

  public static LispSymbolName createSymbolName(Project project, String name) {
    final LispFile file = createFile(project, name);
    return ((LispPrefixedSexp) file.getFirstChild()).getSexp().getSymbolName();
  }

  public static LispStringContent createStringContent(Project project, String content) {
    final LispFile file = createFile(project, String.format("\"%s\"", content));
    return ((LispPrefixedSexp) file.getFirstChild()).getSexp().getString().getStringContent();
  }

  public static LispFile createFile(Project project, String text) {
    String name = "dummy.lisp";
    return (LispFile) PsiFileFactory.getInstance(project)
        .createFileFromText(name, LispFileType.INSTANCE, text);
  }

}
