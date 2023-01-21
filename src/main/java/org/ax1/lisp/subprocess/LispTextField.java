package org.ax1.lisp.subprocess;

import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.ex.EditorEx;
import com.intellij.openapi.project.Project;
import com.intellij.ui.EditorTextField;
import com.intellij.ui.JBColor;
import com.intellij.ui.LanguageTextField.SimpleDocumentCreator;
import org.ax1.lisp.LispLanguage;
import org.jetbrains.annotations.NotNull;

import java.awt.*;

public class LispTextField extends EditorTextField {

  public LispTextField(Project project, String text) {
    super(getDocument(project, text), project, LispLanguage.INSTANCE.getAssociatedFileType(), false, false);
    setBackground(new JBColor(0xf0fff0, 0xf0fff0));
  }

  private static Document getDocument(Project project, String text) {
    SimpleDocumentCreator documentCreator = new SimpleDocumentCreator();
    return documentCreator.createDocument(text, LispLanguage.INSTANCE, project);
  }

  @Override
  public Dimension getMaximumSize() {
    Dimension preferredSize = getPreferredSize();
    return new Dimension(Integer.MAX_VALUE, preferredSize.height);
  }

  @Override
  protected boolean shouldHaveBorder() {
    return false;
  }

  @Override
  protected @NotNull EditorEx createEditor() {
    EditorEx editor = super.createEditor();
    editor.setViewer(true);
    return editor;
  }
}
