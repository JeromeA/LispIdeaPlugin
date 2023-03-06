package org.ax1.lisp.subprocess.action;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.project.Project;
import org.ax1.lisp.subprocess.ExternalDocumentation;
import org.jetbrains.annotations.NotNull;

public class UpdateDocumentation extends AnAction implements DumbAware {

  public UpdateDocumentation() {
    super("Update Documentation From SBCL", "Connect to SBCL, and fetch documentation for COMMON-LISP package.", null);
  }

  @Override
  public void update(@NotNull AnActionEvent e) {
    super.update(e);
  }

  @Override
  public void actionPerformed(@NotNull AnActionEvent e) {
    Project project = e.getProject();
    if (project != null) {
      ExternalDocumentation.getInstance(project).getBindings();
    }
  }

}
