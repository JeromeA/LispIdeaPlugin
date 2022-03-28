package org.ax1.lisp.subprocess;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.CharBuffer;

/** Class to handle in input stream and execute code whenever new characters are read. */
public class InputStreamHandler implements Runnable {

  private InputStream inputStream;

  public InputStreamHandler(InputStream inputStream) {
    this.inputStream = inputStream;
    new Thread(this).start();
  }

  void onInput(String input) {
  }

  void onEof() {
  }

  void onError(String errorMessage) {
  }

  @Override
  public void run() {
    char[] buffer = new char[512];
    InputStreamReader processStdout = new InputStreamReader(inputStream);
    try {
      while (true) {
        int count = processStdout.read(buffer);
        if (count == -1) {
          onEof();
          return;
        };
        onInput(new String(buffer, 0, count));
      }
    } catch (IOException e) {
      onError(e.getMessage());
    }
  }
}
