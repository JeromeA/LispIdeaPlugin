package org.ax1.lisp.subprocess;

import com.intellij.ui.Gray;
import com.intellij.ui.JBColor;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

import static javax.swing.SwingUtilities.invokeLater;

public class InteractionView extends JPanel {

  private final Interaction interaction;
  private final FullWidthTextArea expression;
  private final FullWidthTextArea stdout;
  private final FullWidthTextArea stderr;
  private final FullWidthTextArea error;
  private final FullWidthTextArea result;

  public InteractionView(Interaction interaction) {
    this.interaction = interaction;
    setBorder(BorderFactory.createLineBorder(JBColor.lightGray, 1));
    setBackground(JBColor.PanelBackground);
    setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
    expression = createTextArea();
    expression.setText(interaction.getExpression());
    expression.setBackground(new JBColor(0xf0fff0, 0xf0fff0));
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
