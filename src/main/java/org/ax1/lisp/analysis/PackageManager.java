package org.ax1.lisp.analysis;

import com.intellij.openapi.components.Service;
import com.intellij.openapi.project.Project;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

@Service(Service.Level.PROJECT)
public final class PackageManager {
  private static final Collection<String> COMMON_LISP_FUNCTIONS = Set.of(
      "+", "-", "<", "<=", ">", ">=", "adjust-array", "and", "ash", "assoc", "cdr", "char-code",
      "characterp", "declare", "elt", "eq", "equal", "error", "eval", "format", "funcall", "if",
      "gethash", "ignore", "incf", "integerp", "length", "list", "make-hash-table", "member",
      "not", "null", "return", "second", "setf", "setq", "special", "symbol-value", "symbolp",
      "unless", "when");
  private static final Collection<String> COMMON_LISP_VARIABLES = Set.of("nil", "t");

  private final Map<String, Package> packages = new HashMap<>();

  public static PackageManager getInstance(Project project) {
    return project.getService(PackageManager.class);
  }

  public PackageManager() {
    Package commonLisp = new Package(Set.of("common-lisp", "cl"), Set.of());
    COMMON_LISP_FUNCTIONS.forEach(commonLisp::addFunction);
    COMMON_LISP_VARIABLES.forEach(commonLisp::addVariable);
    add(commonLisp);
    Package commonLispUser = new Package(Set.of("common-lisp-user", "cl-user"), Set.of(commonLisp));
    add(commonLispUser);
  }

  public Package get(String name) {
    return packages.get(name);
  }

  public void add(Package packageToAdd) {
    packageToAdd.getNames().forEach(name -> packages.put(name, packageToAdd));
  }
}
