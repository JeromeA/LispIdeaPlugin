package org.ax1.lisp.analysis.symbol;

import org.ax1.lisp.psi.impl.LispStringDesignator;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

public class Definition {
  protected final Set<LispStringDesignator> usages = new HashSet<>();
  protected final Set<LispStringDesignator> definitions = new HashSet<>();
  protected String descriptionString;

  public void setDescriptionString(String description) {
    this.descriptionString = descriptionString;
  }

  public String getDescriptionString() {
    return descriptionString;
  }

  public Set<LispStringDesignator> getDefinitions() {
    return definitions;
  }

  public LispStringDesignator getDefinition() {
    if (definitions.isEmpty()) return null;
    return definitions.iterator().next();
  }

  public boolean isDefinition(LispStringDesignator symbolName) {
    return definitions.contains(symbolName);
  }

  public Collection<LispStringDesignator> getUsages() {
    return usages;
  }

  public boolean isUsage(LispStringDesignator symbolName) {
    return usages.contains(symbolName);
  }
}
