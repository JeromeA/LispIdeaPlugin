package org.ax1.lisp.psi;

import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiFileFactory;
import com.intellij.psi.PsiWhiteSpace;
import org.ax1.lisp.LispFileType;

public class LispElementFactory {

  public static LispSymbolName createSymbolName(Project project, String name) {
    final LispFile file = createFile(project, name);
    return ((LispPrefixedSexp) file.getFirstChild()).getSexp().getSymbolName();
  }

  public static LispStringContent createStringContent(Project project, String content) {
    final LispFile file = createFile(project, String.format("\"%s\"", content));
    return ((LispPrefixedSexp) file.getFirstChild()).getSexp().getString().getStringContent();
  }

  public static LispPrefixedSexp createSexp(Project project, String text) {
    final LispFile file = createFile(project, text);
    return ((LispPrefixedSexp) file.getFirstChild());
  }

  public static PsiWhiteSpace createNewline(Project project) {
    final LispFile file = createFile(project, "\n");
    return ((PsiWhiteSpace) file.getFirstChild());
  }

  public static LispFile createFile(Project project, String text) {
    String name = "dummy.lisp";
    return (LispFile) PsiFileFactory.getInstance(project)
        .createFileFromText(name, LispFileType.INSTANCE, text);
  }

}
