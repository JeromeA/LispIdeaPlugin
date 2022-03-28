package org.ax1.lisp.refactoring;

import com.intellij.lang.refactoring.NamesValidator;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;

import java.util.regex.Pattern;

public class LispNamesValidator implements NamesValidator {

  public static final Pattern SYMBOL_PATTERN = Pattern.compile("[^() \r\n\t]*");

  @Override
  public boolean isKeyword(@NotNull String name, Project project) {
    return false;
  }

  @Override
  public boolean isIdentifier(@NotNull String name, Project project) {
    return SYMBOL_PATTERN.matcher(name).matches();
  }
}
