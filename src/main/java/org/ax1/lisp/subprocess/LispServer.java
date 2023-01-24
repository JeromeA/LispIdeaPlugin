package org.ax1.lisp.subprocess;

import com.google.common.io.Resources;
import com.intellij.ide.plugins.cl.PluginClassLoader;
import com.intellij.openapi.components.Service;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindowManager;
import org.ax1.lisp.settings.LispSettingsState;
import org.ax1.lisp.subprocess.interaction.Interaction;
import org.ax1.lisp.subprocess.interaction.InteractionManager;

import java.io.*;
import java.net.InetAddress;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.intellij.openapi.components.Service.Level.PROJECT;

@Service(PROJECT)
public final class LispServer {

  private static final String[] LISP_SERVER_SOURCES = { "lisp/package.lisp", "lisp/evaluate.lisp", "lisp/server.lisp" };
  private Process process;
  private Socket socket;
  private int serverPort;
  private final AtomicBoolean serverReady = new AtomicBoolean();
  private final InteractionManager interactionManager;
  private Project project;

  public static LispServer getInstance(Project project) {
    return project.getService(LispServer.class);
  }

  public LispServer(Project project) {
    this.project = project;
    interactionManager = new InteractionManager(project);
  }

  /** Start the subprocess server if necessary, and block until it's ready. */
  @SuppressWarnings("UnstableApiUsage")
  private void ensureProcessRunning() {
    if (process != null && process.isAlive()) return;
    try {
      System.err.println("Starting Lisp process");
      String executable = LispSettingsState.getInstance().selectedBinaryPath;
      process = Runtime.getRuntime().exec(executable);
      Pattern portPattern = Pattern.compile(".* listening on port (\\d+)\n");
      StreamConsumer stdout = new StreamConsumer("Lisp process stdout stream", process.getInputStream(), line -> {
        Matcher portMatcher = portPattern.matcher(line);
        if (portMatcher.matches()) {
          synchronized (serverReady) {
            serverPort = Integer.parseInt(portMatcher.group(1));
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
    System.err.println("Waiting for the server to be ready.");
    synchronized (serverReady) {
      while (!serverReady.get()) {
        try {
          serverReady.wait();
        } catch (InterruptedException e) {
          e.printStackTrace();
        }
      }
    }
    System.err.println("Server is ready.");
  }

  public void evaluate(String expression) {
    Interaction interaction = new Interaction(expression);
    interactionManager.add(interaction);
    ToolWindowManager.getInstance(project).getToolWindow("Lisp").show();
  }

  public InteractionManager getInteractionManager() {
    return interactionManager;
  }

  public Socket getSocket() {
    ensureProcessRunning();
    try {
      if (socket == null || !socket.isConnected()) {
        socket = new Socket(InetAddress.getLoopbackAddress(), serverPort);
      }
      return socket;
    } catch (IOException e) {
      throw new RuntimeException("Fatal: could not get Lisp Server socket.");
    }
  }
}
