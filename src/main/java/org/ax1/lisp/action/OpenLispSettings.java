package org.ax1.lisp.action;

import com.intellij.icons.AllIcons;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.options.ShowSettingsUtil;
import org.ax1.lisp.settings.LispSettingsConfigurable;
import org.jetbrains.annotations.NotNull;

public class OpenLispSettings extends AnAction {

  public OpenLispSettings() {
    super("Edit Lisp Settings", "Edit Lisp settings, including path of Lisp executable.", AllIcons.General.Settings);
  }

  @Override
  public void update(@NotNull AnActionEvent e) {
  }

  @Override
  public void actionPerformed(@NotNull AnActionEvent e) {
    ShowSettingsUtil.getInstance().showSettingsDialog(null, LispSettingsConfigurable.class);
  }
}
