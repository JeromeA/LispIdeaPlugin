package org.ax1.lisp.subprocess.interaction;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.Socket;

/** Run an Interaction against the LispServer, and feed it with results. */
public class InteractionRunner extends Thread {

  private final Socket socket;
  private final Interaction interaction;
  private String currentSection;

  public static void run(Socket socket, Interaction interaction) {
    new InteractionRunner(socket, interaction).start();
  }

  private InteractionRunner(Socket socket, Interaction interaction) {
    super("Interaction runner");
    this.socket = socket;
    this.interaction = interaction;
  }

  @Override
  public synchronized void start() {
    try {
      socket.getOutputStream().write((interaction.getExpression() + "\n--\n").getBytes());
      BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(socket.getInputStream()));
      while (true) {
        String data = bufferedReader.readLine();
        if (data == null) throw new IOException("Unexpected EOF");
        if (!lineReceived(data + "\n")) break;
      }
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  private boolean lineReceived(String line) {
    if (line.startsWith("--")) {
      currentSection = line;
      return !line.equals("--\n");
    }
    switch (currentSection) {
      case "--Result--\n":
        interaction.addResult(line);
        break;
      case "--Error--\n":
        interaction.addError(line);
        break;
      case "--stdout--\n":
        interaction.addStdout(line);
        break;
      case "--stderr--\n":
        interaction.addStderr(line);
        break;
      default:
        throw new RuntimeException("Invalid section");
    }
    return true;
  }

}
