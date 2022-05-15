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
public final class ProjectAnalyser {

  private final Project project;
  private final CachedValuesManager cacheManager;

  public static ProjectAnalyser getInstance(Project project) {
    return project.getService(ProjectAnalyser.class);
  }

  public ProjectAnalyser(Project project) {
    this.project = project;
    cacheManager = CachedValuesManager.getManager(this.project);
  }

  public Collection<PackageDefinition> getPackages() {
    return getCachedValue(project, new ProjectPackageAnalyzer(project));
  }

  public Collection<PackageDefinition> getPackages(LispFile lispFile) {
    return getCachedValue(lispFile, new FilePackageAnalyzer(lispFile));
  }

  public ProjectSymbolAnalysis getProjectSymbolAnalysis() {
    return getCachedValue(project, new ProjectSymbolAnalyzer(project));
  }

  public FileSymbolAnalysis getFileSymbolAnalysis(LispFile lispFile) {
    return getCachedValue(lispFile, new FileSymbolAnalyzer(lispFile));
  }

  private <T> T getCachedValue(UserDataHolder dataHolder, CachedValueProvider<T> provider) {
    return cacheManager.getCachedValue(dataHolder, provider);
  }
}
