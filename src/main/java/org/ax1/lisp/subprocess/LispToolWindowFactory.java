package org.ax1.lisp.subprocess;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowFactory;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.content.Content;
import com.intellij.ui.content.ContentManager;
import org.ax1.lisp.subprocess.interaction.InteractionListView;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class LispToolWindowFactory implements ToolWindowFactory {

  @Override
  public void createToolWindowContent(@NotNull Project project, @NotNull ToolWindow toolWindow) {
    ContentManager contentManager = toolWindow.getContentManager();
    Content interactions = contentManager.getFactory().createContent(createInteractionListComponent(project), "Interactions", false);
    contentManager.addContent(interactions);
    Content logs = contentManager.getFactory().createContent(createLogComponent(project), "Process Logs", false);
    contentManager.addContent(logs);
  }

  private JComponent createLogComponent(@NotNull Project project) {
    FullWidthTextArea textArea = new FullWidthTextArea();
    LispServer.getInstance(project).setStreamListener(textArea::append);
    return new JBScrollPane(textArea);
  }

  @NotNull
  private JComponent createInteractionListComponent(@NotNull Project project) {
    return new InteractionListView(project, LispServer.getInstance(project).getInteractionManager());
  }
}
