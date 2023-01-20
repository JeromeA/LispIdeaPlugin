package org.ax1.lisp.subprocess;

import com.intellij.ui.JBColor;
import com.intellij.ui.components.JBScrollPane;

import javax.swing.*;
import java.util.List;

import static javax.swing.Box.createVerticalGlue;
import static javax.swing.SwingUtilities.invokeLater;

public class InteractionListView extends JBScrollPane {

  private final JPanel contentPanel;
  private final InteractionList interactionList;

  public InteractionListView(InteractionList interactionList) {
    this.interactionList = interactionList;
    contentPanel = new JPanel();
    contentPanel.setBackground(JBColor.PanelBackground);
    contentPanel.setLayout(new BoxLayout(contentPanel, BoxLayout.Y_AXIS));
    contentPanel.setBorder(BorderFactory.createEmptyBorder(4, 4, 4, 4));
    setViewportView(contentPanel);
    interactionList.addListener(l -> invokeLater(this::updateChildren));
    updateChildren();
  }

  private void updateChildren() {
    List<Interaction> interactions = interactionList.getInteractions();
    for (int index = contentPanel.getComponentCount() / 2; index < interactions.size() ; index++) {
      contentPanel.add(new InteractionView(interactions.get(index)));
      contentPanel.add(Box.createVerticalStrut(4));
    }
    validate();
    JScrollBar scrollBar = getVerticalScrollBar();
    scrollBar.setValue(scrollBar.getMaximum());
  }
}