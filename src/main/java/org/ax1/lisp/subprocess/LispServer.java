package org.ax1.lisp.subprocess;

import com.google.common.io.Resources;
import com.intellij.ide.plugins.IdeaPluginDescriptor;
import com.intellij.ide.plugins.PluginManagerCore;
import com.intellij.ide.plugins.cl.PluginClassLoader;
import com.intellij.openapi.components.Service;
import com.intellij.openapi.extensions.PluginId;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindowManager;
import org.ax1.lisp.settings.LispSettingsState;
import org.ax1.lisp.subprocess.interaction.Interaction;
import org.ax1.lisp.subprocess.interaction.InteractionManager;

import java.io.*;
import java.net.InetAddress;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.intellij.openapi.components.Service.Level.PROJECT;

@Service(PROJECT)
public final class LispServer {

  private static final String[] LISP_SERVER_SOURCES = {
      "lisp/package.lisp", "lisp/evaluate.lisp", "lisp/server.lisp", "lisp/get-documentation.lisp"
  };
  private Process process;
  private Socket socket;
  private int serverPort;
  private final AtomicBoolean serverReady = new AtomicBoolean();
  private final InteractionManager interactionManager;
  private final Project project;
  private StreamListener streamListener;

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
      String bootstrapPath = getBootstrapPath();
      process = Runtime.getRuntime().exec(executable + " --load " + bootstrapPath);
      Pattern portPattern = Pattern.compile(".* listening on port (\\d+)\n");
      StreamConsumer stdout = new StreamConsumer("Lisp process stdout stream", process.getInputStream(), line -> {
        log(line);
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
      new StreamConsumer("Lisp process stderr stream", process.getErrorStream(), line -> {
        log(line);
        System.err.println(line);
      }).start();

      send("(idea-server:run-server)\n");

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

  private String getBootstrapPath() {
    PluginId pluginId = PluginId.getId("org.ax1.LispIdeaPlugin");
    IdeaPluginDescriptor plugin = PluginManagerCore.getPlugin(pluginId);
    Path pluginPath = plugin.getPath().toPath();
    Path lispPath = pluginPath.resolve("lisp-idea-server");
    if (!Files.exists(lispPath)) {
      try {
        Files.createDirectories(lispPath);
      } catch (IOException e) {
        throw new RuntimeException(e);
      }
      List<String> files = Arrays.asList("bootstrap.lisp", "package.lisp", "evaluate.lisp", "server.lisp");
      for (String filename : files) {
        copyResourceToFile("lisp/" + filename, lispPath.resolve(filename));
      }
    }
    return lispPath.resolve("bootstrap.lisp").toString();
  }

  private static void copyResourceToFile(String resourcePath, Path outputPath) {
    try (InputStream is = LispServer.class.getClassLoader().getResourceAsStream(resourcePath)) {
      if (is == null) {
        System.out.println("Resource not found: " + resourcePath);
        return;
      }
      Files.copy(is, outputPath, StandardCopyOption.REPLACE_EXISTING);
      System.out.println("Copied " + resourcePath + " to " + outputPath);
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  private void send(String line) throws IOException {
    log("Sending: " + line);
    OutputStream outputStream = process.getOutputStream();
    outputStream.write(line.getBytes());
    outputStream.flush();
  }

  public Interaction evaluate(String expression, boolean visible) {
    log("Evaluating expression: " + expression + "\n");
    Interaction interaction = new Interaction(expression, visible);
    interactionManager.add(interaction);
    if (visible) {
      ToolWindowManager.getInstance(project).getToolWindow("Lisp").show();
    }
    return interaction;
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

  private void log(String line) {
    if (streamListener == null) return;
    streamListener.log(line);
  }

  public void setStreamListener(StreamListener streamListener) {
    this.streamListener = streamListener;
  }

  public interface StreamListener {
    void log(String line);
  }
}
