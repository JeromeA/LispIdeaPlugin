package org.ax1.lisp.subprocess;

import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

public class Interaction {
  private final String expression;
  private String result = "";
  private String error = "";
  private String stdout = "";
  private String stderr = "";
  private final List<ChangeListener> listeners = new ArrayList<>();

  public Interaction(String expression) {
    this.expression = expression;
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
    return stdout;
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
    stdout += line;
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

  interface ChangeListener {
    void interactionChanged(Interaction interaction);
  }
}
