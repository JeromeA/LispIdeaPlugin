package org.ax1.lisp.subprocess;

import com.google.common.io.Resources;
import com.intellij.ide.plugins.cl.PluginClassLoader;
import com.intellij.openapi.components.Service;
import com.intellij.openapi.project.Project;
import org.ax1.lisp.settings.LispSettingsState;

import java.io.*;
import java.net.InetAddress;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.atomic.AtomicBoolean;

import static com.intellij.openapi.components.Service.Level.PROJECT;

@Service(PROJECT)
public final class LispServer {

  private static final String[] LISP_SERVER_SOURCES = { "lisp/package.lisp", "lisp/evaluate.lisp", "lisp/server.lisp" };
  private static final int LISP_SERVER_PORT = 8080;
  private Process process;
  private Socket socket;
  private final AtomicBoolean serverReady = new AtomicBoolean();
  private final InteractionList interactionList = new InteractionList();
  private Interaction currentInteraction;
  private String currentSection;

  public static LispServer getInstance(Project project) {
    return project.getService(LispServer.class);
  }

  @SuppressWarnings("UnstableApiUsage")
  private void ensureProcessRunning() {
    if (process != null && process.isAlive()) return;
    try {
      System.err.println("Starting Lisp process");
      String executable = LispSettingsState.getInstance().selectedBinaryPath;
      process = Runtime.getRuntime().exec(executable);
      StreamConsumer stdout = new StreamConsumer("Lisp process stdout stream", process.getInputStream(), line -> {
        if (line.endsWith("Server is ready\n")) {
          synchronized (serverReady) {
            serverReady.set(true);
            serverReady.notify();
          }
        }
      });
      stdout.start();
      new StreamConsumer("Lisp process stderr stream", process.getErrorStream(), System.err::println).start();
      OutputStream outputStream = process.getOutputStream();

      PluginClassLoader classLoader = (PluginClassLoader) LispServer.class.getClassLoader();
      for (String source : LISP_SERVER_SOURCES) {
        String code = Resources.toString(classLoader.getResource(source), StandardCharsets.UTF_8);
        outputStream.write(code.getBytes());
      }
      outputStream.write("(lisp-idea-plugin:run-server)\n".getBytes());
      outputStream.flush();
      if (socket != null) socket.close();
      socket = null;
    } catch (IOException e) {
      e.printStackTrace();
    }
    System.err.println("Waiting for the server to be ready");
    synchronized (serverReady) {
      while (!serverReady.get()) {
        try {
          serverReady.wait();
        } catch (InterruptedException e) {
          e.printStackTrace();
        }
      }
    }
    System.err.println("Server ready!!!");
  }

  public void evaluate(String expression) {
    evaluate(new Interaction(expression));
  }

  public InteractionList getInteractionList() {
    return interactionList;
  }

  private void evaluate(Interaction interaction) {
    try {
      ensureConnection();
      currentInteraction = interaction;
      interactionList.add(interaction);
      socket.getOutputStream().write((interaction.getExpression() + "\n").getBytes());
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  private void ensureConnection() {
    ensureProcessRunning();
    try {
      if (socket != null && socket.isConnected()) return;
      socket = new Socket(InetAddress.getLoopbackAddress(), LISP_SERVER_PORT);
      new StreamConsumer("Lisp server stdout stream", socket.getInputStream(), this::lineReceived).start();
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  private void lineReceived(String line) {
    if (line.startsWith("--")) {
      currentSection = line;
      if (line.equals("--\n")) {
        currentInteraction = null;
      }
      return;
    }
    switch (currentSection) {
      case "--Result--\n":
        currentInteraction.addResult(line);
        break;
      case "--Error--\n":
        currentInteraction.addError(line);
        break;
      case "--stdout--\n":
        currentInteraction.addStdout(line);
        break;
      case "--stderr--\n":
        currentInteraction.addStderr(line);
        break;
      default:
        throw new RuntimeException("Invalid section");
    }
  }
}
