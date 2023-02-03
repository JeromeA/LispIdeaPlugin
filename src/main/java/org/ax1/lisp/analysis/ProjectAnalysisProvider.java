package org.ax1.lisp.analysis;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Streams;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiManager;
import com.intellij.psi.search.FileTypeIndex;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.util.CachedValueProvider;
import com.intellij.psi.util.PsiModificationTracker;
import org.ax1.lisp.LispFileType;
import org.ax1.lisp.analysis.symbol.PackageDefinition;
import org.ax1.lisp.psi.LispFile;
import org.ax1.lisp.subprocess.ExternalDocumentation;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;

import static com.google.common.collect.ImmutableList.toImmutableList;

class ProjectAnalysisProvider implements CachedValueProvider<ProjectDefinitions> {

  private final Project project;
  private final ProjectComputedData projectComputedData;
  private final PsiManager psiManager;
  private final ExternalDocumentation externalDocumentation;

  public ProjectAnalysisProvider(Project project) {
    this.project = project;
    externalDocumentation = ExternalDocumentation.getInstance(project);
    projectComputedData = ProjectComputedData.getInstance(project);
    psiManager = PsiManager.getInstance(project);
  }

  @Override
  public @Nullable Result<ProjectDefinitions> compute() {
    ImmutableList<Bindings> bindings = Streams.concat(
            getFileDefinitions().stream(),
            getExternalDocumentation().stream())
        .collect(toImmutableList());
    return new Result<>(new ProjectDefinitions(bindings, getPackageDefinitions()),
        PsiModificationTracker.MODIFICATION_COUNT, externalDocumentation.getModificationTracker());
  }

  private ImmutableList<Bindings> getExternalDocumentation() {
    return ImmutableList.of(externalDocumentation.getBindings());
  }

  private Collection<PackageDefinition> getPackageDefinitions() {
    return projectComputedData.getPackageDefinitions();
  }

  private ImmutableList<Bindings> getFileDefinitions() {
    return FileTypeIndex.getFiles(LispFileType.INSTANCE, GlobalSearchScope.allScope(project)).stream()
        .map(psiManager::findFile)
        .map(f -> (LispFile) f)
        .map(projectComputedData::getFileAnalysis)
        .collect(toImmutableList());
  }
}
