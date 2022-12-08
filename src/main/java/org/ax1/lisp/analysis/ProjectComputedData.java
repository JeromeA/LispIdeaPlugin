package org.ax1.lisp.analysis;

import com.intellij.openapi.components.Service;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.UserDataHolder;
import com.intellij.psi.util.CachedValueProvider;
import com.intellij.psi.util.CachedValuesManager;
import org.ax1.lisp.analysis.symbol.PackageDefinition;
import org.ax1.lisp.psi.LispFile;

import java.util.*;

@Service
public final class ProjectComputedData {

  private final Project project;
  private final CachedValuesManager cacheManager;

  public static ProjectComputedData getInstance(Project project) {
    return project.getService(ProjectComputedData.class);
  }

  public ProjectComputedData(Project project) {
    this.project = project;
    cacheManager = CachedValuesManager.getManager(this.project);
  }

  public Collection<PackageDefinition> getPackageDefinitions() {
    return getCachedValue(project, new PackageBindingProvider(project));
  }

  public Bindings getPackageBindings(LispFile lispFile) {
    return getCachedValue(lispFile, new FilePackageAnalyzer(lispFile));
  }

  public synchronized ProjectDefinitions getProjectAnalysis() {
    return getCachedValue(project, new ProjectAnalysisProvider(project));
  }

  public Bindings getFileAnalysis(LispFile lispFile) {
    return getCachedValue(lispFile, new FileAnalysisProvider(lispFile));
  }

  private <T> T getCachedValue(UserDataHolder dataHolder, CachedValueProvider<T> provider) {
    return cacheManager.getCachedValue(dataHolder, provider);
  }
}
