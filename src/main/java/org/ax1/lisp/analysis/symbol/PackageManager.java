package org.ax1.lisp.analysis.symbol;

import org.ax1.lisp.analysis.LocatedSymbol;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;

import java.util.*;

public final class PackageManager {

  public final Map<String, LispPackage> packages = new HashMap<>();
  private LispPackage currentPackage;

  public PackageManager() {
    add(KeywordPackage.INSTANCE);
    add(CommonLispPackage.INSTANCE);
    CommonLispUserPackage commonLispUser = new CommonLispUserPackage(this);
    add(commonLispUser);
    currentPackage = commonLispUser;
  }

  public PackageManager(Collection<PackageDefinition> packages) {
    this();
    packages.stream().map(definition -> new LispPackage(this, definition)).forEach(this::add);
  }

  public LispPackage getPackage(String name) {
    return packages.get(name);
  }

  private LispPackage getOrCreatePackage(String name) {
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

  public Symbol getSymbol(LispSexp parsedSymbol) {
    return getSymbol(parsedSymbol.getText());
  }

  public LocatedSymbol getLocatedSymbol(LispSexp parsedSymbol) {
    return new LocatedSymbol(getSymbol(parsedSymbol.getText()), parsedSymbol);
  }

  public Symbol getSymbol(String name) {
    name = name.toUpperCase();
    if (name.startsWith("#:")) {
      return new Symbol("", name.substring(2));
    }
    if (name.startsWith(":")) {
      return KeywordPackage.INSTANCE.intern(name.substring(1));
    }
    int doubleColon = name.indexOf("::");
    if (doubleColon > 0) {
      String packageName = name.substring(0, doubleColon);
      String symbolName = name.substring(doubleColon + 2);
      LispPackage lispPackage = getOrCreatePackage(packageName);
      // If we were Lisp, we would call findSymbol, which can return null. But we want to be able to manipulate
      // that unknown symbol, find all its occurrences, etc, so we really want it to exist. If we need to mark it as
      // invalid, this will have to be done at a later stage.
      return lispPackage.intern(symbolName);
    }
    int colon = name.indexOf(":");
    if (colon > 0) {
      String packageName = name.substring(0, colon);
      String symbolName = name.substring(colon + 1);
      LispPackage lispPackage = getOrCreatePackage(packageName);
      // If we were Lisp, we would call findExportedSymbol, which can return null. But we want to be able to manipulate
      // that unknown symbol, so we really want it to exist.
      return lispPackage.intern(symbolName);
    }
    return currentPackage.intern(name);
  }

  public void setCurrentPackage(String name) {
    currentPackage = getOrCreatePackage(name);
  }
}
