package org.ax1.lisp.subprocess.interaction;

import com.intellij.openapi.project.Project;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

public class InteractionManager extends Thread {

  private static final int QUEUE_CAPACITY = 10;

  private final List<Interaction> interactions = new ArrayList<>();
  private final List<ChangeListener> listeners = new ArrayList<>();
  private final BlockingQueue<Interaction> interactionsToRun = new ArrayBlockingQueue<>(QUEUE_CAPACITY);
  private final InteractionRunner runner;

  public InteractionManager(Project project) {
    super("Interaction manager");
    runner = new InteractionRunner(project);
    start();
  }

  public void add(Interaction interaction) {
    interactions.add(interaction);
    interactionsToRun.add(interaction);
    fireChanged();
  }

  @SuppressWarnings("InfiniteLoopStatement")
  @Override
  public synchronized void run() {
    while(true) {
      try {
        runner.runInteraction(interactionsToRun.take());
      } catch (InterruptedException ignored) {
      }
    }
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
