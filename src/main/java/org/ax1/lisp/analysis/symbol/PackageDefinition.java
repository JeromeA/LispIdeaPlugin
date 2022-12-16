package org.ax1.lisp.analysis.symbol;

import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;

import java.util.*;

public class PackageDefinition extends LispDefinition {
  final String name;
  private final Set<String> nicknames = new HashSet<>();
  private String description;
  public final Map<String, LispSexp> use = new HashMap<>();
  public final Map<String, LispSexp> exports = new HashMap<>();
  public final Map<String, LispSexp> shadows = new HashMap<>();
  public final Map<LispSymbol, Set<LispSymbol>> importFrom = new HashMap<>();

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

  public void setDefinition(LispSexp symbol) {
    definitions.add(symbol);
  }

  @Override
  public String getName() {
    return name;
  }

  public void addExport(String symbolName) {
    exports.put(symbolName, null);
  }

  public void setDescription(String description) {
    this.description = description;
  }
}
