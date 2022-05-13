package org.ax1.lisp;

import com.intellij.openapi.components.Service;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiManager;
import com.intellij.psi.search.FileTypeIndex;
import com.intellij.psi.search.GlobalSearchScope;
import org.ax1.lisp.analysis.Annotate;
import org.ax1.lisp.analysis.LexicalBindingManager;
import org.ax1.lisp.analysis.PackageAnalyzer;
import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.analysis.symbol.PackageDefinition;
import org.ax1.lisp.analysis.symbol.SymbolManager;
import org.ax1.lisp.psi.LispFile;

import java.util.*;
import java.util.stream.Collectors;

@Service
public final class LispProject {

  public static Annotate EMPTY_ANNOTATE = new Annotate(null, null);

  private SymbolManager symbolManager;
  private Collection<PackageDefinition> packages;
  private final Project project;
  private final Map<LispFile, Set<PackageDefinition>> filePackages = new HashMap<>();
  final Map<LispFile, SymbolManager> fileSymbols = new HashMap<>();

  public static LispProject getInstance(Project project) {
    return project.getService(LispProject.class);
  }

  public LispProject(Project project) {
    this.project = project;
  }

  public Collection<PackageDefinition> getPackages() {
    analyzePackages();
    return packages;
  }

  public SymbolManager getSymbolManager() {
    analyzeSymbols();
    return symbolManager;
  }

  private void analyzePackages() {
    removeInvalidPackages();
    List<LispFile> filesToUpdate = getLispFiles().stream()
        .filter(lispFile -> !filePackages.containsKey(lispFile)).collect(Collectors.toList());
    if (filesToUpdate.isEmpty()) return;
    filesToUpdate.forEach(this::analyzePackagesForFile);
    packages = filePackages.values().stream().flatMap(Collection::stream).collect(Collectors.toSet());
  }

  private void removeInvalidPackages() {
    List<LispFile> invalidFiles = filePackages.keySet().stream()
        .filter(lispFile -> !lispFile.isValid())
        .collect(Collectors.toList());
    invalidFiles.forEach(filePackages::remove);
  }

  private void analyzeSymbols() {
    List<LispFile> invalidFiles = fileSymbols.keySet().stream()
        .filter(lispFile -> !lispFile.isValid())
        .collect(Collectors.toList());
    invalidFiles.forEach(fileSymbols::remove);
    getLispFiles().stream()
        .filter(lispFile -> !fileSymbols.containsKey(lispFile))
        .forEach(this::analyzeSymbolsForFile);
    symbolManager = SymbolManager.merge(fileSymbols.values());
  }

  private void analyzeSymbolsForFile(LispFile lispFile) {
    analyzeSymbolsForFile(lispFile, EMPTY_ANNOTATE);
  }

  public LexicalBindingManager analyzeSymbolsForFile(LispFile lispFile, Annotate annotate) {
    SyntaxAnalyzer analyzer = new SyntaxAnalyzer(lispFile, annotate, new SymbolManager(getPackages()));
    analyzer.analyze();
    fileSymbols.put(lispFile, analyzer.symbolManager);
    if (!analyzer.lexicalBindings.isEmpty()) throw new RuntimeException("Unbalanced lexical stack.");
    return analyzer.lexicalBindings;
  }

  private void analyzePackagesForFile(LispFile lispFile) {
    analyzePackagesForFile(lispFile, EMPTY_ANNOTATE);
  }

  public void analyzePackagesForFile(LispFile lispFile, Annotate annotate) {
    PackageAnalyzer packageAnalyzer = new PackageAnalyzer(lispFile, annotate);
    packageAnalyzer.analyzePackages();
    filePackages.put(lispFile, packageAnalyzer.analyzer.packages);
  }

  private Set<LispFile> getLispFiles() {
    PsiManager psiManager = PsiManager.getInstance(project);
    return FileTypeIndex.getFiles(LispFileType.INSTANCE, GlobalSearchScope.allScope(project)).stream()
        .map(psiManager::findFile)
        .filter(Objects::nonNull)
        .map(LispFile.class::cast)
        .collect(Collectors.toSet());
  }
}
