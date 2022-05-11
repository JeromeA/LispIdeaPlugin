package org.ax1.lisp.analysis.symbol;

import java.util.HashSet;
import java.util.Set;

public class PackageDefinition {
  final String name;
  private final Set<String> nicknames = new HashSet<>();
  final Set<String> use = new HashSet<>();
  private boolean isWriteable = true;
  private boolean isStandard;

  public PackageDefinition(String name) {
    this.name = name;
  }

  public void addNickname(String nickname) {
    nicknames.add(nickname);
  }

  public Set<String> getNicknames() {
    return nicknames;
  }

  public void addUse(String packageName) {
    use.add(packageName);
  }

  public boolean isWriteable() {
    return isWriteable;
  }

  public void setReadOnly() {
    isWriteable = false;
  }

  public boolean isStandard() {
    return isStandard;
  }

  public void setStandard() {
    isStandard = true;
  }

}
