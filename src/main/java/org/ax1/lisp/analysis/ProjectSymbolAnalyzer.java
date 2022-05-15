package org.ax1.lisp.analysis;

import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiManager;
import com.intellij.psi.search.FileTypeIndex;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.util.CachedValueProvider;
import com.intellij.psi.util.PsiModificationTracker;
import org.ax1.lisp.LispFileType;
import org.ax1.lisp.analysis.symbol.SymbolManager;
import org.ax1.lisp.psi.LispFile;
import org.jetbrains.annotations.Nullable;

import java.util.Set;
import java.util.stream.Collectors;

class ProjectSymbolAnalyzer implements CachedValueProvider<SymbolManager> {

  private final Project project;
  private final ProjectAnalyser projectAnalyser;
  private final PsiManager psiManager;

  public ProjectSymbolAnalyzer(Project project) {
    this.project = project;
    projectAnalyser = ProjectAnalyser.getInstance(project);
    psiManager = PsiManager.getInstance(project);
  }

  @Override
  public @Nullable Result<SymbolManager> compute() {
    Set<SymbolManager> symbolManagers =
        FileTypeIndex.getFiles(LispFileType.INSTANCE, GlobalSearchScope.allScope(project)).stream()
            .map(psiManager::findFile)
            .map(f -> (LispFile) f)
            .map(projectAnalyser::getSymbolManager)
            .collect(Collectors.toSet());
    return new Result<>(SymbolManager.merge(symbolManagers), PsiModificationTracker.MODIFICATION_COUNT);
  }
}
