package org.ax1.lisp.subprocess;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowFactory;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.content.Content;
import com.intellij.ui.content.ContentManager;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class LispToolWindowFactory implements ToolWindowFactory {

  @Override
  public void createToolWindowContent(@NotNull Project project, @NotNull ToolWindow toolWindow) {
    ContentManager contentManager = toolWindow.getContentManager();
    Content content = contentManager.getFactory().createContent(createComponent(project), null, false);
    contentManager.addContent(content);
  }

  @NotNull
  private JComponent createComponent(@NotNull Project project) {
    return new InteractionListView(project, LispServer.getInstance(project).getInteractionList());
  }
}
