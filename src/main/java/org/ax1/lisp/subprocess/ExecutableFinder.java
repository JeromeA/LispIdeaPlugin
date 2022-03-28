package org.ax1.lisp.subprocess;

import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static java.util.function.Predicate.not;

/** A class to find executables in the PATH. */
public class ExecutableFinder {

  List<String> extraPaths;
  Map<String, String> env = System.getenv();
  Predicate<File> fileExists = File::exists;

  private ExecutableFinder() {
  }

  public static ExecutableFinder create() {
    return new ExecutableFinder();
  }

  /** Inject an alternative environment, for testing purposes. */
  public ExecutableFinder withEnv(Map<String, String> env) {
    this.env = env;
    return this;
  }

  /** Inject an alternative file testing predicate, for testing purposes. */
  public ExecutableFinder withFileExists(Predicate<File> fileExists) {
    this.fileExists = fileExists;
    return this;
  }

  /** Add extra paths to be checked after thoses from $PATH. */
  public ExecutableFinder withExtraPaths(List<String> extraPaths) {
    this.extraPaths = extraPaths;
    return this;
  }

  /** Find the full pathname for the given binary filenames. */
  public List<String> find(List<String> filenames) {
    List<String> result = new ArrayList<>();
    getPaths().stream()
        .flatMap(path -> filenames.stream().map(name -> new File(path, name)))
        .filter(fileExists)
        .map(File::getAbsolutePath)
        .forEach(result::add);
    return result;
  }

  @NotNull
  private List<String> getPaths() {
    List<String> paths = Arrays.stream(env.get("PATH").split(":")).collect(Collectors.toList());
    if (extraPaths != null) extraPaths.stream().filter(not(paths::contains)).forEach(paths::add);
    return paths;
  }
}
