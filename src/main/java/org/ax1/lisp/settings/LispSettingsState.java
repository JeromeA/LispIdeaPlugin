package org.ax1.lisp.settings;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.util.xmlb.XmlSerializerUtil;
import org.ax1.lisp.subprocess.ExecutableFinder;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

@State(
    name = "org.ax1.lisp.settings.LispSettingsState",
    storages = @Storage("Ax1LispPlugin.xml")
)
public class LispSettingsState implements PersistentStateComponent<LispSettingsState> {

  public List<String> binaryPaths = new ArrayList<>();
  public String selectedBinaryPathName = null;

  public LispSettingsState() {
    setDefaultValues();
  }

  public static LispSettingsState getInstance() {
    return ApplicationManager.getApplication().getService(LispSettingsState.class);
  }

  @Override
  public @Nullable LispSettingsState getState() {
    return this;
  }

  @Override
  public void loadState(@NotNull LispSettingsState state) {
    XmlSerializerUtil.copyBean(state, this);
  }

  private void setDefaultValues() {
    binaryPaths = ExecutableFinder.create()
        .withExtraPaths(List.of("/usr/bin", "/usr/local/bin"))
        .find(List.of("sbcl", "clisp", "ecl"));
    selectedBinaryPathName = binaryPaths.isEmpty() ? "" : binaryPaths.get(0);
  }
}
