package org.ax1.lisp;

import com.intellij.openapi.components.Service;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiManager;
import com.intellij.psi.search.FileTypeIndex;
import com.intellij.psi.search.GlobalSearchScope;
import org.ax1.lisp.analysis.Annotate;
import org.ax1.lisp.analysis.PackageAnalyzer;
import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.analysis.symbol.Package;
import org.ax1.lisp.analysis.symbol.SymbolManager;
import org.ax1.lisp.psi.LispFile;

import java.util.*;
import java.util.stream.Collectors;

@Service
public final class LispProject {

  private Annotate EMPTY_ANNOTATE = new Annotate(null, null);

  private SymbolManager symbolManager;
  private Collection<Package> packages;
  private final Project project;
  private final Map<LispFile, Set<Package>> filePackages = new HashMap();
  private final Map<LispFile, SymbolManager> fileSymbols = new HashMap();

  public static LispProject getInstance(Project project) {
    return project.getService(LispProject.class);
  }

  public LispProject(Project project) {
    this.project = project;
  }

  public SymbolManager getSymbolManager() {
    updateSymbolManager();
    return symbolManager;
  }

  private void updateSymbolManager() {
    updatePackages();
    updateSymbols();
  }

  public void updatePackages() {
    List<LispFile> invalidFiles = filePackages.keySet().stream()
        .filter(lispFile -> !lispFile.isValid())
        .collect(Collectors.toList());
    invalidFiles.forEach(filePackages::remove);
    getLispFiles().stream()
        .filter(lispFile -> !filePackages.containsKey(lispFile))
        .forEach(this::updatePackagesForFile);
    packages = filePackages.values().stream().flatMap(Collection::stream).collect(Collectors.toSet());
  }

  private void updateSymbols() {
    List<LispFile> invalidFiles = fileSymbols.keySet().stream()
        .filter(lispFile -> !lispFile.isValid())
        .collect(Collectors.toList());
    invalidFiles.forEach(fileSymbols::remove);
    getLispFiles().stream()
        .filter(lispFile -> !fileSymbols.containsKey(lispFile))
        .forEach(lispFile -> updateSymbolsForFile(lispFile, packages));
    symbolManager = SymbolManager.mergeBindings(fileSymbols.values());
  }

  private void updateSymbolsForFile(LispFile lispFile, Collection<Package> packages) {
    SyntaxAnalyzer analyzer = new SyntaxAnalyzer(lispFile, EMPTY_ANNOTATE, new SymbolManager(packages));
    analyzer.analyze();
    setSymbols(lispFile, analyzer.symbolManager);
  }

  private void updatePackagesForFile(LispFile lispFile) {
    PackageAnalyzer packageAnalyzer = new PackageAnalyzer(lispFile, EMPTY_ANNOTATE);
    packageAnalyzer.analyzePackages();
    setPackages(lispFile, packageAnalyzer.analyzer.symbolManager.getUserDefinedPackages());
  }

  private Set<LispFile> getLispFiles() {
    PsiManager psiManager = PsiManager.getInstance(project);
    return FileTypeIndex.getFiles(LispFileType.INSTANCE, GlobalSearchScope.allScope(project)).stream()
        .map(psiManager::findFile)
        .filter(Objects::nonNull)
        .map(LispFile.class::cast)
        .collect(Collectors.toSet());
  }

  public void setPackages(LispFile lispFile, Set<Package> packages) {
    filePackages.put(lispFile, packages);
  }

  public void setSymbols(LispFile lispFile, SymbolManager symbolManager) {
    fileSymbols.put(lispFile, symbolManager);
  }

  public Collection<Package> getPackages() {
    updatePackages();
    return packages;
  }
}
