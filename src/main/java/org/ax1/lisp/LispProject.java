package org.ax1.lisp;

import com.intellij.openapi.components.Service;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiManager;
import com.intellij.psi.search.FileTypeIndex;
import com.intellij.psi.search.GlobalSearchScope;
import org.ax1.lisp.analysis.Annotate;
import org.ax1.lisp.analysis.PackageAnalyzer;
import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.analysis.symbol.LispPackage;
import org.ax1.lisp.analysis.symbol.SymbolBinding;
import org.ax1.lisp.analysis.symbol.SymbolManager;
import org.ax1.lisp.psi.LispFile;

import java.util.*;
import java.util.stream.Collectors;

@Service
public final class LispProject {

  public static Annotate EMPTY_ANNOTATE = new Annotate(null, null);

  private SymbolManager symbolManager;
  private Collection<LispPackage> packages;
  private final Project project;
  private final Map<LispFile, Set<LispPackage>> filePackages = new HashMap();
  final Map<LispFile, SymbolManager> fileSymbols = new HashMap();
  final Map<LispFile, Collection<SymbolBinding>> lexicalBindings = new HashMap();

  public static LispProject getInstance(Project project) {
    return project.getService(LispProject.class);
  }

  public LispProject(Project project) {
    this.project = project;
  }

  public SymbolManager getSymbolManager() {
    updatePackages();
    updateSymbols();
    return symbolManager;
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

  private void updateSymbolsForFile(LispFile lispFile, Collection<LispPackage> packages) {
    SyntaxAnalyzer analyzer = new SyntaxAnalyzer(lispFile, EMPTY_ANNOTATE, new SymbolManager(packages));
    analyzer.analyze();
    fileSymbols.put(lispFile, analyzer.symbolManager);
    lexicalBindings.put(lispFile, analyzer.lexicalBindings.getRetired());
  }

  private void updatePackagesForFile(LispFile lispFile) {
    PackageAnalyzer packageAnalyzer = new PackageAnalyzer(lispFile, EMPTY_ANNOTATE);
    packageAnalyzer.analyzePackages();
    filePackages.put(lispFile, packageAnalyzer.analyzer.symbolManager.getUserDefinedPackages());
  }

  private Set<LispFile> getLispFiles() {
    PsiManager psiManager = PsiManager.getInstance(project);
    return FileTypeIndex.getFiles(LispFileType.INSTANCE, GlobalSearchScope.allScope(project)).stream()
        .map(psiManager::findFile)
        .filter(Objects::nonNull)
        .map(LispFile.class::cast)
        .collect(Collectors.toSet());
  }

  public Collection<LispPackage> getPackages() {
    updatePackages();
    return packages;
  }
}
