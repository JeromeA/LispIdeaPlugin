package org.ax1.lisp.analysis.symbol;

import java.util.*;

/**
 * A container for the list of known packages, created for the duration of a syntax analysis.
 */
public final class PackageManager {

  public final Map<String, LispPackage> packages = new HashMap<>();

  public PackageManager() {
    add(KeywordPackage.INSTANCE);
    add(CommonLispPackage.INSTANCE);
    CommonLispUserPackage commonLispUser = new CommonLispUserPackage(this);
    add(commonLispUser);
  }

  public PackageManager(Collection<PackageDefinition> packages) {
    this();
    packages.stream().map(definition -> new LispPackage(this, definition)).forEach(this::add);
  }

  public LispPackage getPackage(String name) {
    return packages.get(name);
  }

  public LispPackage getOrCreatePackage(String name) {
    if (!packages.containsKey(name)) {
      packages.put(name, new LispPackage(this, PackageDefinition.createDefaultDefinition(name)));
    }
    return packages.get(name);
  }

  public void add(LispPackage packageToAdd) {
    if (packages.containsKey(packageToAdd.getName())) {
      System.err.println("Duplicate package " + packageToAdd.getName() + " ignored.");
      return;
    }
    addImpl(packageToAdd);
  }

  private void addImpl(LispPackage packageToAdd) {
    packages.put(packageToAdd.getName(), packageToAdd);
    for (String name : packageToAdd.getDefinition().getNicknames()) {
      packages.put(name, packageToAdd);
    }
  }
}
