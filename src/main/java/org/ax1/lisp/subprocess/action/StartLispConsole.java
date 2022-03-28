package org.ax1.lisp.subprocess.action;

import com.intellij.icons.AllIcons;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.project.DumbAware;
import org.ax1.lisp.subprocess.LispConsole;
import org.jetbrains.annotations.NotNull;

public class StartLispConsole extends AnAction implements DumbAware {

  public StartLispConsole() {
    super("Lisp Console", "Start lisp read-eval-print loop.", AllIcons.Actions.Execute);
  }

  @Override
  public void actionPerformed(@NotNull AnActionEvent e) {
    LispConsole.getInstance(e.getProject()).start();
  }
}
