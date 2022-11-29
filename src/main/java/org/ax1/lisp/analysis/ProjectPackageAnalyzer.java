package org.ax1.lisp.analysis;

import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiManager;
import com.intellij.psi.search.FileTypeIndex;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.util.CachedValueProvider;
import com.intellij.psi.util.PsiModificationTracker;
import org.ax1.lisp.LispFileType;
import org.ax1.lisp.analysis.symbol.PackageDefinition;
import org.ax1.lisp.psi.LispFile;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;
import java.util.Set;
import java.util.stream.Collectors;

public class ProjectPackageAnalyzer implements CachedValueProvider<Collection<PackageDefinition>> {

  private final Project project;
  private final ProjectComputedData projectComputedData;
  private final PsiManager psiManager;

  public ProjectPackageAnalyzer(Project project) {
    this.project = project;
    projectComputedData = ProjectComputedData.getInstance(project);
    psiManager = PsiManager.getInstance(project);
  }

  @Override
  public @Nullable Result<Collection<PackageDefinition>> compute() {
    Set<PackageDefinition> packages =
        FileTypeIndex.getFiles(LispFileType.INSTANCE, GlobalSearchScope.allScope(project)).stream()
            .map(psiManager::findFile)
            .map(f -> (LispFile) f)
            .map(projectComputedData::getPackages)
            .flatMap(Collection::stream).collect(Collectors.toSet());
    return new Result<>(packages, PsiModificationTracker.MODIFICATION_COUNT);
  }
}
