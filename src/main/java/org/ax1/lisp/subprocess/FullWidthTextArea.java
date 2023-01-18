package org.ax1.lisp.subprocess;

import com.intellij.ui.JBColor;

import javax.swing.*;
import java.awt.*;

public class FullWidthTextArea extends JTextArea {

  public FullWidthTextArea() {
    setEditable(false);
    setBorder(BorderFactory.createLineBorder(JBColor.PanelBackground));
  }

  @Override
  public void setText(String text) {
    text = text.replaceAll("\n$", "");
    if (text.isEmpty()) {
      setVisible(false);
    } else {
      super.setText(text);
      setVisible(true);
    }
  }

  @Override
  public Dimension getMaximumSize() {
    Dimension preferredSize = getPreferredSize();
    return new Dimension(Integer.MAX_VALUE, preferredSize.height);
  }
}
