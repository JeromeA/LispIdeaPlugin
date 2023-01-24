package org.ax1.lisp.subprocess.interaction;

import com.intellij.openapi.project.Project;
import com.intellij.ui.JBColor;
import com.intellij.ui.components.JBScrollPane;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static javax.swing.SwingUtilities.invokeLater;

public class InteractionListView extends JBScrollPane {

  private final Project project;
  private final JPanel contentPanel;
  private final InteractionManager interactionManager;
  private final Set<Interaction> displayed = new HashSet<>();

  public InteractionListView(@NotNull Project project, InteractionManager interactionManager) {
    this.project = project;
    this.interactionManager = interactionManager;
    contentPanel = new JPanel();
    contentPanel.setBackground(JBColor.PanelBackground);
    contentPanel.setLayout(new BoxLayout(contentPanel, BoxLayout.Y_AXIS));
    contentPanel.setBorder(BorderFactory.createEmptyBorder(4, 4, 4, 4));
    setViewportView(contentPanel);
    interactionManager.addListener(() -> invokeLater(this::updateChildren));
    updateChildren();
  }

  private void updateChildren() {
    List<Interaction> interactions = interactionManager.getInteractions();
    for (Interaction interaction : interactions) {
      if (!interaction.isVisible() || displayed.contains(interaction)) continue;
      displayed.add(interaction);
      contentPanel.add(new InteractionView(project, interaction));
      contentPanel.add(Box.createVerticalStrut(4));
    }
    validate();
    JScrollBar scrollBar = getVerticalScrollBar();
    scrollBar.setValue(scrollBar.getMaximum());
  }
}
