package org.ax1.lisp.subprocess.interaction;

import com.intellij.openapi.project.Project;
import org.ax1.lisp.subprocess.LispServer;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.Socket;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

/** Run an Interaction against the LispServer, and feed it with results. */
public class InteractionRunner extends Thread {

  private static final int QUEUE_CAPACITY = 10;

  private static Project project;
  private final BlockingQueue<Interaction> interactionsToRun = new ArrayBlockingQueue<>(QUEUE_CAPACITY);
  private String currentSection;

  public InteractionRunner(Project project) {
    super("Interaction runner");
    InteractionRunner.project = project;
    start();
  }

  @SuppressWarnings("InfiniteLoopStatement")
  @Override
  public synchronized void run() {
    while(true) {
      try {
        runInteraction(interactionsToRun.take());
      } catch (InterruptedException ignored) {
      }
    }
  }

  public void queue(Interaction interaction) {
    interactionsToRun.add(interaction);
  }

  public void runInteraction(Interaction interaction) {
    String separator = null;
    try {
      Socket socket = LispServer.getInstance(project).getSocket();
      socket.getOutputStream().write((interaction.getExpression() + "\n--\n").getBytes());
      BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(socket.getInputStream()));
      while (true) {
        String line = bufferedReader.readLine();
        if (line == null) throw new IOException("Unexpected EOF");
        if (separator == null) {
          separator = line.substring(0, line.indexOf("Result"));
        }
        if (line.startsWith(separator)) {
          currentSection = line.substring(separator.length());
          if (currentSection.isEmpty()) return;
          continue;
        }
        line = line + "\n";
        switch (currentSection) {
          case "Result":
            interaction.addResult(line);
            break;
          case "Error":
            interaction.addError(line);
            break;
          case "stdout":
            interaction.addStdout(line);
            break;
          case "stderr":
            interaction.addStderr(line);
            break;
          default:
            throw new RuntimeException("Invalid section");
        }
      }
    } catch (IOException e) {
      e.printStackTrace();
    } finally {
      interaction.markAsComplete();
    }
  }
}
