package org.ax1.lisp.subprocess;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

import java.util.List;
import java.util.Map;

import static com.google.common.truth.Truth.assertThat;

@RunWith(JUnit4.class)
public class ExecutableFinderTest {

  @Test
  public void empty() {
    assertThat(ExecutableFinder.create().find(List.of())).isEmpty();
  }

  @Test
  public void withAllMissingFiles() {
    assertThat(
        ExecutableFinder.create()
            .withEnv(Map.of("PATH", "/path1:/path2"))
            .withFileExists(f -> false)
            .find(List.of("name1", "name2")))
        .isEmpty();
  }

  @Test
  public void withAllExistingFile() {
    assertThat(
        ExecutableFinder.create()
            .withEnv(Map.of("PATH", "/path1:/path2"))
            .withFileExists(f -> true)
            .find(List.of("name1", "name2")))
        .containsExactly("/path1/name1", "/path1/name2", "/path2/name1", "/path2/name2");
  }

  @Test
  public void withExtraPaths() {
    assertThat(
        ExecutableFinder.create()
            .withEnv(Map.of("PATH", "/path1"))
            .withExtraPaths(List.of("/path2"))
            .withFileExists(f -> true)
            .find(List.of("name1", "name2")))
        .containsExactly("/path1/name1", "/path1/name2", "/path2/name1", "/path2/name2");
  }
}
