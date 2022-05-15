package org.ax1.lisp.analysis;

import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.analysis.symbol.SymbolBinding;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

public class FileSymbolAnalysis {
  public Map<Symbol, SymbolBinding> functions = new HashMap<>();
  public Map<Symbol, SymbolBinding> variables = new HashMap<>();
  public Collection<SymbolBinding> retiredBindings;
}
