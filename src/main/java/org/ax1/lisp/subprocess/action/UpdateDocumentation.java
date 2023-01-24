package org.ax1.lisp.subprocess.action;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.project.DumbAware;
import org.ax1.lisp.subprocess.LispServer;
import org.ax1.lisp.subprocess.interaction.Interaction;
import org.jetbrains.annotations.NotNull;

public class UpdateDocumentation extends AnAction implements DumbAware {

  public UpdateDocumentation() {
    super("Update Documentation From SBCL", "Connect to SBCL, and fetch documentation for COMMON-LISP package.", null);
  }

  @Override
  public void actionPerformed(@NotNull AnActionEvent e) {
    Interaction interaction = LispServer.getInstance(e.getProject()).evaluate("(lisp-idea-plugin:get-documentation)", false);
    System.err.println("UpdateDocumentation Interaction: " + interaction);
  }

}
