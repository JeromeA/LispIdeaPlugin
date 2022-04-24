package org.ax1.lisp.analysis.symbol;

import com.intellij.openapi.components.Service;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import static org.ax1.lisp.analysis.symbol.SymbolBinding.BindingType.DYNAMIC;
import static org.ax1.lisp.analysis.symbol.SymbolBinding.SymbolType.FUNCTION;
import static org.ax1.lisp.analysis.symbol.SymbolBinding.SymbolType.VARIABLE;

@Service(Service.Level.PROJECT)
public final class SymbolManager {

  private final Map<String, Package> packages = new HashMap<>();
  private final KeywordPackage keywordPackage = new KeywordPackage();
  private Package currentPackage;
  private final Map<Symbol, SymbolBinding> functions = new HashMap<>();
  private final Map<Symbol, SymbolBinding> variables = new HashMap<>();

  public static SymbolManager getInstance(Project project) {
    return project.getService(SymbolManager.class);
  }

  public SymbolManager() {
    Package commonLisp = new CommonLispPackage(this);
    add(commonLisp);
    Package commonLispUser = new Package("COMMON-LISP-USER");
    commonLispUser.addUse(commonLisp);
    commonLispUser.setNicknames(Set.of("CL-USER"));
    add(commonLispUser);
    add(keywordPackage);
    currentPackage = commonLispUser;
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
      return keywordPackage.intern(name.substring(1));
    }
    if (index > 0) {
      // TODO: handle double colon.
      String packageName = name.substring(0, index);
      String symbolName = name.substring(index + 1);
      return packages.get(packageName).intern(symbolName);
    }
    return currentPackage.intern(name);
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
}
