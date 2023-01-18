package org.ax1.lisp.subprocess;

import java.util.ArrayList;
import java.util.List;

public class InteractionList {

  private List<Interaction> interactions = new ArrayList<>();
  private final List<ChangeListener> listeners = new ArrayList<>();

  public void add(Interaction interaction) {
    interactions.add(interaction);
    fireChanged();
  }

  public void addListener(ChangeListener listener) {
    listeners.add(listener);
  }

  private void fireChanged() {
    listeners.forEach(l -> l.interactionChanged(this));
  }

  public List<Interaction> getInteractions() {
    return interactions;
  }

  interface ChangeListener {
    void interactionChanged(InteractionList interactionList);
  }
}
