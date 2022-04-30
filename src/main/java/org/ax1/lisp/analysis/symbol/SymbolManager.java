package org.ax1.lisp.analysis.symbol;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;
import org.jetbrains.annotations.NotNull;

import java.util.*;
import java.util.stream.Collectors;

import static org.ax1.lisp.analysis.symbol.SymbolBinding.BindingType.DYNAMIC;
import static org.ax1.lisp.analysis.symbol.SymbolBinding.SymbolType.FUNCTION;
import static org.ax1.lisp.analysis.symbol.SymbolBinding.SymbolType.VARIABLE;

public final class SymbolManager {

  private static final KeywordPackage keywordPackage = new KeywordPackage();

  public final Map<String, Package> packages = new HashMap<>();
  private final Map<Symbol, SymbolBinding> functions = new HashMap<>();
  private final Map<Symbol, SymbolBinding> variables = new HashMap<>();
  private Package currentPackage;

  public SymbolManager() {
    add(new CommonLispPackage(this));
    Package commonLispUser = new CommonLispUserPackage();
    add(commonLispUser);
    add(keywordPackage);
    currentPackage = commonLispUser;
  }

  public SymbolManager(Collection<Package> packages) {
    this();
    packages.forEach(this::add);
  }

  public static SymbolManager mergeBindings(Collection<SymbolManager> symbolManagers) {
    SymbolManager result = new SymbolManager(symbolManagers.iterator().next().packages.values());
    Multimap<Symbol, SymbolBinding> functions = ArrayListMultimap.create();
    symbolManagers.stream().flatMap(symbolManager -> symbolManager.functions.entrySet().stream())
        .forEach(entry -> functions.put(entry.getKey(), entry.getValue()));
    functions.keySet().forEach(symbol -> {
      Collection<SymbolBinding> bindings = functions.get(symbol);
      result.functions.put(symbol, SymbolBinding.merge(bindings));
    });
    Multimap<Symbol, SymbolBinding> variables = ArrayListMultimap.create();
    symbolManagers.stream().flatMap(symbolManager -> symbolManager.variables.entrySet().stream())
        .forEach(entry -> variables.put(entry.getKey(), entry.getValue()));
    variables.keySet().forEach(symbol -> {
      Collection<SymbolBinding> bindings = variables.get(symbol);
      result.variables.put(symbol, SymbolBinding.merge(bindings));
    });
    return result;
  }

  public Package getPackage(String name) {
    return packages.get(name);
  }

  public void add(Package packageToAdd) {
    packages.put(packageToAdd.getName(), packageToAdd);
    packageToAdd.getNicknames().forEach(name -> packages.put(name, packageToAdd));
  }

  public Symbol getSymbol(String name) {
    name = name.toUpperCase();
    int index = name.indexOf(':');
    if (index == 0) {
      return keywordPackage.intern(this, name.substring(1));
    }
    if (index > 0) {
      // TODO: handle double colon.
      String packageName = name.substring(0, index);
      String symbolName = name.substring(index + 1);
      return packages.get(packageName).intern(this, symbolName);
    }
    return currentPackage.intern(this, name);
  }

  public SymbolBinding getFunction(Symbol symbol) {
    return getBinding(functions, symbol, FUNCTION);
  }

  public SymbolBinding getFunction(String symbolName) {
    return getFunction(getSymbol(symbolName));
  }

  public SymbolBinding getVariable(Symbol symbol) {
    return getBinding(variables, symbol, VARIABLE);
  }

  public SymbolBinding getVariable(String symbolName) {
    return getVariable(getSymbol(symbolName));
  }

  public void setCurrentPackage(Package newPackage) {
    currentPackage = newPackage;
  }

  public Collection<SymbolBinding> getFunctions() {
    return functions.values();
  }

  public Collection<SymbolBinding> getVariables() {
    return variables.values();
  }

  @NotNull
  private SymbolBinding getBinding(Map<Symbol, SymbolBinding> map, Symbol symbol, SymbolBinding.SymbolType symbolType) {
    SymbolBinding binding = map.get(symbol);
    if (binding == null) {
      binding = new SymbolBinding(symbol.getName(), symbolType, DYNAMIC);
      map.put(symbol, binding);
    }
    return binding;
  }

  public List<Symbol> getAvailableFunctions() {
    return getAvailableSymbols().stream().filter(functions::containsKey).collect(Collectors.toList());
  }

  public List<Symbol> getAvailableVariables() {
    return getAvailableSymbols().stream().filter(variables::containsKey).collect(Collectors.toList());
  }

  private Collection<Symbol> getAvailableSymbols() {
    return currentPackage.getSymbols();
  }

  public Set<Package> getUserDefinedPackages() {
    return packages.values().stream()
        .filter(p -> !p.isStandardPackage())
        .collect(Collectors.toSet());
  }
}
