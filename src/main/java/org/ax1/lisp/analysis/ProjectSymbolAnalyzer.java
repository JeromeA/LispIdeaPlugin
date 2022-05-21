package org.ax1.lisp.analysis;

import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiManager;
import com.intellij.psi.search.FileTypeIndex;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.util.CachedValueProvider;
import com.intellij.psi.util.PsiModificationTracker;
import org.ax1.lisp.LispFileType;
import org.ax1.lisp.psi.LispFile;
import org.jetbrains.annotations.Nullable;

class ProjectSymbolAnalyzer implements CachedValueProvider<ProjectSymbolAnalysis> {

  private final Project project;
  private final ProjectAnalyser projectAnalyser;
  private final PsiManager psiManager;

  public ProjectSymbolAnalyzer(Project project) {
    this.project = project;
    projectAnalyser = ProjectAnalyser.getInstance(project);
    psiManager = PsiManager.getInstance(project);
  }

  @Override
  public @Nullable Result<ProjectSymbolAnalysis> compute() {
    ProjectSymbolAnalysis.Builder result = ProjectSymbolAnalysis.newBuilder();
    FileTypeIndex.getFiles(LispFileType.INSTANCE, GlobalSearchScope.allScope(project)).stream()
        .map(psiManager::findFile)
        .map(f -> (LispFile) f)
        .map(projectAnalyser::getFileSymbolAnalysis)
        .forEach(result::addFileAnalysis);
    projectAnalyser.getPackages().forEach(result::addPackage);
    return new Result<>(result.build(), PsiModificationTracker.MODIFICATION_COUNT);
  }
}
