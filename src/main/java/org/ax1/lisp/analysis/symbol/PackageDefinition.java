package org.ax1.lisp.analysis.symbol;

import org.ax1.lisp.psi.impl.LispStringDesignator;

import java.util.*;

public class PackageDefinition {

  // For both in-package and symbol prefixes.
  protected final Set<LispStringDesignator> usages = new HashSet<>();
  protected final List<LispStringDesignator> definitions = new ArrayList<>();
  final String name;
  private final Set<String> nicknames = new HashSet<>();
  private String description;
  public final Map<String, LispStringDesignator> use = new HashMap<>();
  public final Map<String, LispStringDesignator> exports = new HashMap<>();
  public final Map<String, LispStringDesignator> shadows = new HashMap<>();
//  public final Map<LispSymbol, Set<LispSymbol>> importFrom = new HashMap<>();

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

  public void setDescription(String description) {
    this.description = description;
  }

  public List<LispStringDesignator> getDefinitions() {
    return definitions;
  }

  public LispStringDesignator getDefinition() {
    if (definitions.isEmpty()) return null;
    return definitions.get(0);
  }

  public boolean isDefinition(LispStringDesignator packageName) {
    return definitions.contains(packageName);
  }

  public Collection<LispStringDesignator> getUsages() {
    return usages;
  }

  public boolean isUsage(LispStringDesignator packageName) {
    return usages.contains(packageName);
  }
}
