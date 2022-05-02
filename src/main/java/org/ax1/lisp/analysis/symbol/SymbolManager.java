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

  public final Map<String, LispPackage> packages = new HashMap<>();
  private final Map<Symbol, SymbolBinding> functions = new HashMap<>();
  private final Map<Symbol, SymbolBinding> variables = new HashMap<>();
  private LispPackage currentPackage;
  private CommonLispUserPackage commonLispUser;

  public SymbolManager() {
    add(new CommonLispPackage(this));
    commonLispUser = new CommonLispUserPackage();
    add(commonLispUser);
    add(keywordPackage);
    currentPackage = commonLispUser;
  }

  public SymbolManager(Collection<LispPackage> packages) {
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

  public LispPackage getPackage(String name) {
    return packages.get(name);
  }

  public CommonLispUserPackage getCommonLispUserPackage() {
    return commonLispUser;
  }

  public void add(LispPackage packageToAdd) {
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

  public void setCurrentPackage(LispPackage newPackage) {
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

  public List<Symbol> getAvailableFunctions(LispPackage lispPackage) {
    return getAvailableSymbols(lispPackage).stream().filter(functions::containsKey).collect(Collectors.toList());
  }

  public List<Symbol> getAvailableVariables(LispPackage lispPackage) {
    return getAvailableSymbols(lispPackage).stream().filter(variables::containsKey).collect(Collectors.toList());
  }

  private Collection<Symbol> getAvailableSymbols(LispPackage lispPackage) {
    return lispPackage.getSymbols();
  }

  public Set<LispPackage> getUserDefinedPackages() {
    return packages.values().stream()
        .filter(p -> !p.isStandardPackage())
        .collect(Collectors.toSet());
  }

  public LispPackage getCurrentPackage() {
    return currentPackage;
  }
}
