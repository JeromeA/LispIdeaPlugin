package org.ax1.lisp.subprocess.interaction;

import com.intellij.openapi.project.Project;
import com.intellij.ui.Gray;
import com.intellij.ui.JBColor;
import org.ax1.lisp.subprocess.FullWidthTextArea;
import org.ax1.lisp.subprocess.LispTextField;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

import static javax.swing.SwingUtilities.invokeLater;

public class InteractionView extends JPanel {

  private final Interaction interaction;
  private final FullWidthTextArea stdout;
  private final FullWidthTextArea stderr;
  private final FullWidthTextArea error;
  private final FullWidthTextArea result;

  public InteractionView(Project project, Interaction interaction) {
    this.interaction = interaction;
    setBorder(BorderFactory.createLineBorder(JBColor.lightGray, 1));
    setBackground(JBColor.PanelBackground);
    setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
    add(new LispTextField(project, interaction.getExpression()));
    stdout = createTextArea();
    stderr = createTextArea();
    stderr.setBackground(Gray._245);
    error = createTextArea();
    error.setBackground(Gray._235);
    result = createTextArea();
    interaction.addListener(i -> invokeLater(this::updateChildren));
    updateChildren();
  }

  private void updateChildren() {
    stdout.setText(interaction.getStdout());
    stderr.setText(interaction.getStderr());
    error.setText(interaction.getError());
    result.setText(interaction.getResult());
  }

  @NotNull
  private FullWidthTextArea createTextArea() {
    return (FullWidthTextArea) add(new FullWidthTextArea());
  }
}
