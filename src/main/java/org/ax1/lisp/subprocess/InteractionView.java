package org.ax1.lisp.subprocess;

import com.intellij.openapi.util.text.Strings;
import com.intellij.ui.JBColor;

import javax.swing.*;

import static javax.swing.SwingUtilities.invokeLater;

public class InteractionView extends JPanel {

  private final Interaction interaction;

  public InteractionView(Interaction interaction) {
    this.interaction = interaction;
    setBackground(JBColor.PanelBackground);
    setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
    interaction.addListener(i -> invokeLater(this::updateChildren));
    updateChildren();
  }

  private void updateChildren() {
    if (getComponentCount() == 0) {
      add(new FullWidthTextArea(interaction.getExpression()));
      add(Box.createVerticalStrut(1));
    }
    if (getComponentCount() == 2 && !interaction.getResult().isEmpty()) {
      add(new FullWidthTextArea());
      add(Box.createVerticalStrut(1));
    }
    if (!interaction.getResult().isEmpty()) {
      FullWidthTextArea textArea = (FullWidthTextArea) getComponent(2);
      textArea.setText(Strings.join(interaction.getResult(), "\n"));
    }
  }
}
