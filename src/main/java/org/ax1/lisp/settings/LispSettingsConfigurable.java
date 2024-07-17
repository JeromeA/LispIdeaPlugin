package org.ax1.lisp.settings;

import com.intellij.openapi.options.Configurable;

import javax.swing.*;
import java.util.List;
import java.util.Objects;

public class LispSettingsConfigurable implements Configurable {

  private LispSettingsComponent mySettingsComponent;

  @Override
  public String getDisplayName() {
    return "Lisp";
  }

  @Override
  public JComponent getPreferredFocusedComponent() {
    return mySettingsComponent.getPreferredFocusedComponent();
  }

  @Override
  public JComponent createComponent() {
    mySettingsComponent = new LispSettingsComponent();
    return mySettingsComponent.getPanel();
  }

  @Override
  public boolean isModified() {
    LispSettingsState settings = LispSettingsState.getInstance();
    boolean modified = !Objects.equals(mySettingsComponent.getSelectedBinaryPath(), settings.selectedBinaryPathName);
    modified |= !mySettingsComponent.getBinaryPaths().equals(settings.binaryPaths);
    return modified;
  }

  @Override
  public void apply() {
    LispSettingsState settings = LispSettingsState.getInstance();
    settings.selectedBinaryPathName = mySettingsComponent.getSelectedBinaryPath();
    settings.binaryPaths = List.copyOf(mySettingsComponent.getBinaryPaths());
  }

  @Override
  public void reset() {
    LispSettingsState settings = LispSettingsState.getInstance();
    mySettingsComponent.setSelectedBinaryPath(settings.selectedBinaryPathName);
    mySettingsComponent.setBinaryPaths(settings.binaryPaths);
  }

  @Override
  public void disposeUIResources() {
    mySettingsComponent = null;
  }

}
