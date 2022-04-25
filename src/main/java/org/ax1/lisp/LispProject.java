package org.ax1.lisp;

import com.intellij.openapi.components.Service;
import com.intellij.openapi.project.Project;
import org.ax1.lisp.analysis.symbol.SymbolManager;

@Service
public final class LispProject {

  private SymbolManager symbolManager;

  public static LispProject getInstance(Project project) {
    return project.getService(LispProject.class);
  }

  public void setSymbolManager(SymbolManager symbolManager) {
    this.symbolManager = symbolManager;
  }

  public SymbolManager getSymbolManager() {
    return symbolManager;
  }
}
