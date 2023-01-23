package org.ax1.lisp.subprocess.interaction;

import com.intellij.openapi.project.Project;

import java.util.ArrayList;
import java.util.List;

public class InteractionManager {

  private final List<Interaction> interactions = new ArrayList<>();
  private final List<ChangeListener> listeners = new ArrayList<>();
  private final InteractionRunner runner;

  public InteractionManager(Project project) {
    runner = new InteractionRunner(project);
  }

  public void add(Interaction interaction) {
    interactions.add(interaction);
    runner.queue(interaction);
    fireChanged();
  }

  public void addListener(ChangeListener listener) {
    listeners.add(listener);
  }

  private void fireChanged() {
    listeners.forEach(ChangeListener::interactionChanged);
  }

  public List<Interaction> getInteractions() {
    return interactions;
  }

  interface ChangeListener {
    void interactionChanged();
  }
}
