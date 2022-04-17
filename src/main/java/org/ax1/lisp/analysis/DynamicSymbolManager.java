package org.ax1.lisp.analysis;

import com.intellij.openapi.components.Service;
import com.intellij.openapi.project.Project;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSymbol;

import java.util.HashMap;
import java.util.Map;

@Service(Service.Level.PROJECT)
public final class DynamicSymbolManager {
  private final Map<String, DynamicSymbolDescriptor> symbols = new HashMap<>();

  public static DynamicSymbolManager getInstance(Project project) {
    return project.getService(DynamicSymbolManager.class);
  }

  public void declareFunction(LispList container, LispSymbol symbol) {
    getSymbolDescriptor(symbol.getText()).getFunction().setDefinition(container, symbol);
  }

  public void declareVariable(LispList container, LispSymbol symbol) {
    getSymbolDescriptor(symbol.getText()).getVariable().setDefinition(container, symbol);
  }

  public DynamicSymbolDescriptor getSymbolDescriptor(String symbolName) {
    DynamicSymbolDescriptor symbolDescriptor = symbols.get(symbolName);
    if (symbolDescriptor == null) {
      symbolDescriptor = new DynamicSymbolDescriptor(symbolName);
      symbols.put(symbolName, symbolDescriptor);
    }
    return symbolDescriptor;
  }

  public Map<String, DynamicSymbolDescriptor> getSymbols() {
    return symbols;
  }
}
