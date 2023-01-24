package org.ax1.lisp.subprocess.interaction;

import java.util.ArrayList;
import java.util.List;

public class Interaction {

  private static final int MAXIMUM_SIZE = 10000;

  private final String expression;
  private String result = "";
  private String error = "";
  private String stdout = "";
  private String stderr = "";
  private final boolean visible;
  private final List<ChangeListener> listeners = new ArrayList<>();

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
    return stdout + (stdout.length() < MAXIMUM_SIZE ? "" : "[truncated]\n");
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
    if (stdout.length() < MAXIMUM_SIZE) {
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
}
