package org.ax1.lisp.subprocess.interaction;

import java.util.ArrayList;
import java.util.List;

public class Interaction {

  private static final int MAXIMUM_SIZE = 10000;

  private final String expression;
  private String error = "";
  private String stderr = "";
  private String stdout = "";
  private String result = "";
  private final boolean visible;
  private final List<ChangeListener> listeners = new ArrayList<>();
  private boolean completed;

  public Interaction(String expression, boolean visible) {
    this.expression = expression;
    this.visible = visible;
  }

  public String getExpression() {
    return expression;
  }

  public String getResult() {
    return result;
  }

  public String getError() {
    return error;
  }

  public String getStdout() {
    return stdout + (isVisible() || stdout.length() < MAXIMUM_SIZE ? "" : "[truncated]\n");
  }

  public String getStderr() {
    return stderr;
  }

  public void addResult(String line) {
    result += line;
    fireChanged();
  }

  public void addError(String line) {
    error += line;
    fireChanged();
  }

  public void addStdout(String line) {
    if (stdout.length() < MAXIMUM_SIZE || !isVisible()) {
      stdout += line;
    }
    fireChanged();
  }

  public void addStderr(String line) {
    stderr += line;
    fireChanged();
  }

  public void addListener(ChangeListener listener) {
    listeners.add(listener);
  }

  private void fireChanged() {
    listeners.forEach(l -> l.interactionChanged(this));
  }

  public boolean isVisible() {
    return visible;
  }

  interface ChangeListener {
    void interactionChanged(Interaction interaction);
  }

  public synchronized void markAsComplete() {
    completed = true;
    notify();
  }

  public synchronized void waitUntilComplete() {
    while (!completed) {
      try {
        wait();
      } catch (InterruptedException ignored) {
      }
    }
  }

  @Override
  public String toString() {
    return String.format("[expression=\"%.10000s\", error=\"%.10000s\", stderr=\"%.10000s\", stdout=\"%.10000s\", result=\"%.10000s\"]",
        expression, error, stderr, stdout, result);
  }
}
