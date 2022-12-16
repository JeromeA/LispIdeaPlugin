package org.ax1.lisp.analysis.symbol;

import org.ax1.lisp.psi.LispSexp;

import java.util.*;

public abstract class LispDefinition {

  protected final Set<LispSexp> usages = new HashSet<>();
  protected final List<LispSexp> definitions = new ArrayList<>();

  public abstract String getName();

  public List<LispSexp> getDefinitions() {
    return definitions;
  }

  public LispSexp getDefinition() {
    if (definitions.isEmpty()) return null;
    return definitions.get(0);
  }

  public boolean isDefinition(LispSexp sexp) {
    return definitions.contains(sexp);
  }

  public Collection<LispSexp> getUsages() {
    return usages;
  }

  public boolean isUsage(LispSexp sexp) {
    return usages.contains(sexp);
  }
}
