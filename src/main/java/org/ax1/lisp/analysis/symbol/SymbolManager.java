package org.ax1.lisp.analysis.symbol;

import org.jetbrains.annotations.NotNull;

import java.util.*;
import java.util.stream.Collectors;

public final class SymbolManager {

  public static final KeywordPackage keywordPackage = new KeywordPackage();
  public static final CommonLispPackage commonLispPackage = new CommonLispPackage();
  public static final String INVALID_PACKAGE = "INVALID-PACKAGE";

  public final Map<String, LispPackage> packages = new HashMap<>();
  private LispPackage currentPackage;

  public SymbolManager() {
    add(keywordPackage);
    add(commonLispPackage);
    CommonLispUserPackage commonLispUser = new CommonLispUserPackage();
    add(commonLispUser);
    currentPackage = commonLispUser;
  }

  public SymbolManager(Collection<PackageDefinition> packages) {
    this();
    packages.stream().map(LispPackage::new).forEach(this::add);
  }

  public static SymbolManager merge(Collection<SymbolManager> symbolManagers) {
    SymbolManager result = new SymbolManager();
    symbolManagers.stream().flatMap(s -> s.packages.values().stream())
        .filter(p -> p.getDefinition().isWriteable())
        .forEach(result::merge);
    return result;
  }

  public LispPackage getPackage(String name) {
    return packages.get(name);
  }

  public void add(LispPackage packageToAdd) {
    if (packages.containsKey(packageToAdd.getName())) {
      throw new RuntimeException("Package already exists.");
    }
    addImpl(packageToAdd);
  }

  public void merge(LispPackage packageToAdd) {
    LispPackage localPackage = packages.get(packageToAdd.getName());
    if (localPackage == null) {
      localPackage = packageToAdd.clone();
      addImpl(localPackage);
    }
    localPackage.merge(packageToAdd);
  }

  private void addImpl(LispPackage packageToAdd) {
    packages.put(packageToAdd.getName(), packageToAdd);
    for (String name : packageToAdd.getDefinition().getNicknames()) {
      packages.put(name, packageToAdd);
    }
  }

  public Symbol getSymbol(String name) {
    name = name.toUpperCase();
    if (name.startsWith("#:")) {
      return new Symbol("", name);
    }
    int index = name.indexOf(':');
    if (index == 0) {
      return keywordPackage.intern(this, name.substring(1));
    }
    if (index > 0) {
      // TODO: handle double colon.
      String packageName = name.substring(0, index);
      String symbolName = name.substring(index + 1);
      LispPackage lispPackage = packages.get(packageName);
      if (lispPackage == null) {
        return new Symbol(INVALID_PACKAGE, name);
      }
      return lispPackage.intern(this, symbolName);
    }
    return currentPackage.intern(this, name);
  }

  public void setCurrentPackage(LispPackage newPackage) {
    currentPackage = newPackage;
  }

  public LispPackage getCurrentPackage() {
    return currentPackage;
  }

  public SymbolBinding getFunction(String symbolName) {
    return getFunction(getSymbol(symbolName));
  }

  public SymbolBinding getFunction(Symbol symbol) {
    return getPackage(symbol.getPackageName()).getFunction(symbol);
  }

  public SymbolBinding getVariable(String symbolName) {
    return getVariable(getSymbol(symbolName));
  }

  public SymbolBinding getVariable(Symbol symbol) {
    return getPackage(symbol.getPackageName()).getVariable(symbol);
  }

  @NotNull
  public Collection<SymbolBinding> getBindings() {
    return packages.values().stream().flatMap(p -> p.getBindings().stream()).collect(Collectors.toList());
  }
}
