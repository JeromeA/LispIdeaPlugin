package org.ax1.lisp.subprocess;

import com.intellij.ui.components.JBTextArea;

import java.awt.*;

public class FullWidthTextArea extends JBTextArea {

  public FullWidthTextArea(String text) {
    super(text);
    setEditable(false);
  }

  public FullWidthTextArea() {
    setEditable(false);
  }

  @Override
  public Dimension getMaximumSize() {
    Dimension preferredSize = getPreferredSize();
    return new Dimension(Integer.MAX_VALUE, preferredSize.height);
  }
}
