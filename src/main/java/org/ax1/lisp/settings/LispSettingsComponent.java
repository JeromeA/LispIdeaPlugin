package org.ax1.lisp.settings;

import com.intellij.openapi.ui.ComboBox;
import com.intellij.ui.CollectionComboBoxModel;
import com.intellij.ui.components.JBLabel;
import com.intellij.util.ui.FormBuilder;

import javax.swing.*;
import java.util.List;

public class LispSettingsComponent {
  private final JPanel myMainPanel;
  private final CollectionComboBoxModel<String> binaryPathsModel = new CollectionComboBoxModel<>();
  private final ComboBox<String> binaryPaths = new ComboBox<>(binaryPathsModel);

  public LispSettingsComponent() {
    myMainPanel = FormBuilder.createFormBuilder()
        .addLabeledComponent(new JBLabel("Common Lisp binary: "), binaryPaths, 1, false)
        .addComponentFillVertically(new JPanel(), 0)
        .getPanel();
  }

  public JPanel getPanel() {
    return myMainPanel;
  }

  public JComponent getPreferredFocusedComponent() {
    return binaryPaths;
  }

  public List<String> getBinaryPaths() {
    return binaryPathsModel.getItems();
  }

  public void setBinaryPaths(List<String> newPaths) {
    binaryPathsModel.removeAll();
    binaryPathsModel.add(newPaths);
  }

  public String getSelectedBinaryPath() {
    return binaryPathsModel.getSelected();
  }

  public void setSelectedBinaryPath(String newText) {
    binaryPathsModel.setSelectedItem(newText);
  }
}
