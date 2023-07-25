package org.ax1.lisp.analysis.symbol;

import org.ax1.lisp.psi.impl.LispStringDesignator;

import java.util.*;

public class PackageDefinition extends Definition {

  final String name;
  private final Set<String> nicknames = new HashSet<>();
  public final Map<String, LispStringDesignator> use = new HashMap<>();
  public final Map<String, LispStringDesignator> exports = new HashMap<>();
  public final Map<String, LispStringDesignator> shadows = new HashMap<>();
  // Warning: this is a symbol to package map, as this is the way we need to query it.
  public final Map<String, String> importFrom = new HashMap<>();

  private boolean isWriteable = true;

  public PackageDefinition(String name) {
    this.name = name;
  }

  public static PackageDefinition createDefaultDefinition(String name) {
    PackageDefinition definition = new PackageDefinition(name);
    definition.use.put(CommonLispPackage.COMMON_LISP, null);
    return definition;
  }

  public void addNickname(String nickname) {
    nicknames.add(nickname);
  }

  public Set<String> getNicknames() {
    return nicknames;
  }

  public void setReadOnly() {
    isWriteable = false;
  }

  public void setDefinition(LispStringDesignator symbol) {
    definitions.add(symbol);
  }

  public String getName() {
    return name;
  }

  public void addExport(String symbolName) {
    exports.put(symbolName, null);
  }

  public void addImportFrom(String symbolName, String packageName) {
    importFrom.put(symbolName, packageName);
  }
}
