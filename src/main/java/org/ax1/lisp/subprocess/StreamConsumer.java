package org.ax1.lisp.subprocess;

import java.io.*;
import java.util.function.Consumer;

public class StreamConsumer extends Thread {
  private final BufferedReader bufferedReader;
  private Consumer<String> consumer;

  public StreamConsumer(String name, InputStream inputStream, Consumer<String> consumer) {
    setName(name);
    this.bufferedReader = new BufferedReader(new InputStreamReader(inputStream));
    this.consumer = consumer;
  }

  @Override
  public void run() {
    String data;
    while (true) {
      try {
        if ((data = bufferedReader.readLine()) == null) break;
      } catch (IOException e) {
        e.printStackTrace();
        return;
      }
      consumer.accept(data);
    }
  }
}
