package org.ax1.lisp.subprocess.interaction;

import com.intellij.openapi.project.Project;
import org.ax1.lisp.subprocess.LispServer;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.Socket;

/** Run an Interaction against the LispServer, and feed it with results. */
public class InteractionRunner {

  private static Project project;

  public InteractionRunner(Project project) {
    InteractionRunner.project = project;
  }

  /**
   *  Run an interaction and get its results.
   * <p>
   *  The communication protocol with the Lisp Server is as follows:
   *  <ul>
   *    <li> The client sends the expression to evaluate, followed by "\n--\n".
   *    <li> The server replies with a line that contain some random element and will be used as the separator,
   *         for example "--42--\n".
   *    <li> The server sends sections, which start with a section type (stderr, stdout, error, result) line, followed
   *         by the content, followed by a newline and the separator mentioned above (the extra newline is to guarantee
   *         that the separator can be found on its own on a line).
   *    <li> When the result section is finished, the interaction is finished and a new one can start.
   *  </ul>
   */
  public void runInteraction(Interaction interaction) {
    try {
      Socket socket = LispServer.getInstance(project).getSocket();
      socket.getOutputStream().write((interaction.getExpression() + "\n--\n").getBytes());
      BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(socket.getInputStream()));
      String separator = bufferedReader.readLine();
      if (separator == null) throw new IOException("Unexpected EOF");
      while (true) {
        String currentSection = bufferedReader.readLine();
        if (currentSection == null) throw new IOException("Unexpected EOF");
        boolean firstLine = true;
        while (true) {
          String line = bufferedReader.readLine();
          if (line.equals(separator)) break;
          if (!firstLine) line = "\n" + line;
          switch (currentSection) {
            case "result":
              interaction.addResult(line);
              break;
            case "error":
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
          firstLine = false;
        }
        if (currentSection.equals("result")) break;
      }
    } catch (IOException e) {
      e.printStackTrace();
      interaction.addError("Could not evaluate expression: " + e.getMessage());
    } finally {
      interaction.markAsComplete();
    }
  }
}
