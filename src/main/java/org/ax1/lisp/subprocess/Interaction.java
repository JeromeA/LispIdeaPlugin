package org.ax1.lisp.subprocess;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class Interaction {
  private final String expression;
  private final List<String> result = new ArrayList<>();
  private final List<String> error = new ArrayList<>();
  private final List<String> stdout = new ArrayList<>();
  private final List<String> stderr = new ArrayList<>();
  private final List<ChangeListener> listeners = new ArrayList<>();

  public Interaction(String expression) {
    this.expression = expression;
  }

  public String getExpression() {
    return expression;
  }

  public Collection<String> getResult() {
    return result;
  }

  public void addResult(String line) {
    result.add(line);
    fireChanged();
  }

  public void addError(String line) {
    error.add(line);
  }

  public void addStdout(String line) {
    stdout.add(line);
  }

  public void addStderr(String line) {
    stderr.add(line);
  }

  public void addListener(ChangeListener listener) {
    listeners.add(listener);
  }

  private void fireChanged() {
    listeners.forEach(l -> l.interactionChanged(this));
  }

  interface ChangeListener {
    void interactionChanged(Interaction interaction);
  }
}
