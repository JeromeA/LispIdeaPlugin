package org.ax1.lisp.analysis;

import static org.ax1.lisp.analysis.SymbolBinding.BindingType.DYNAMIC;
import static org.ax1.lisp.analysis.SymbolBinding.SymbolType.FUNCTION;
import static org.ax1.lisp.analysis.SymbolBinding.SymbolType.VARIABLE;

public class DynamicSymbolDescriptor {

  private final SymbolBinding function;
  private final SymbolBinding variable;

  public DynamicSymbolDescriptor(String symbolName) {
    variable = new SymbolBinding(symbolName, VARIABLE, DYNAMIC);
    function = new SymbolBinding(symbolName, FUNCTION, DYNAMIC);
  }

  public SymbolBinding getFunction() {
    return function;
  }

  public SymbolBinding getVariable() {
    return variable;
  }
}
