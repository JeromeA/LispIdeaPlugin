package org.ax1.lisp.subprocess;

import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.project.DumbAwareAction;
import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.NotNull;


public class LispLoadFileAction extends DumbAwareAction {
  public static final String ID = "LispLoadFileAction";

  @Override
  public void actionPerformed(@NotNull AnActionEvent e) {
    LispServer lispServer = LispServer.getInstance(e.getProject());
    VirtualFile file = e.getData(CommonDataKeys.VIRTUAL_FILE);
    if (file == null) return;
    lispServer.evaluate(String.format("(load \"%s\")", file.getPath()), true);
  }

  @Override
  public void update(@NotNull AnActionEvent actionEvent) {
    VirtualFile file = actionEvent.getData(CommonDataKeys.VIRTUAL_FILE);
    if (file == null) return;
    actionEvent.getPresentation().setText("Load File " + file.getName());
  }
}
