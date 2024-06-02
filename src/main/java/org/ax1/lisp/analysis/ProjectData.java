package org.ax1.lisp.analysis;

import com.intellij.openapi.components.Service;
import com.intellij.openapi.project.Project;
import com.intellij.psi.stubs.StubIndex;
import com.intellij.psi.stubs.StubIndexKey;
import org.ax1.lisp.analysis.symbol.CommonLispPackage;
import org.ax1.lisp.analysis.symbol.CommonLispUserPackage;
import org.ax1.lisp.analysis.symbol.Package;
import org.ax1.lisp.psi.impl.LispStringDesignator;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.stream.Collectors;

import static org.ax1.lisp.stubs.index.LispClassDefinitionIndex.CLASS_DEFINITIONS;
import static org.ax1.lisp.stubs.index.LispClassUsageIndex.CLASS_USAGES;
import static org.ax1.lisp.stubs.index.LispFunctionDefinitionIndex.FUNCTION_DEFINITIONS;
import static org.ax1.lisp.stubs.index.LispFunctionUsageIndex.FUNCTION_USAGES;
import static org.ax1.lisp.stubs.index.LispMethodDefinitionIndex.METHOD_DEFINITIONS;
import static org.ax1.lisp.stubs.index.LispPackageDefinitionIndex.PACKAGE_DEFINITIONS;
import static org.ax1.lisp.stubs.index.LispPackageUsageIndex.PACKAGE_USAGES;
import static org.ax1.lisp.stubs.index.LispSlotDefinitionIndex.SLOT_DEFINITIONS;
import static org.ax1.lisp.stubs.index.LispSlotUsageIndex.SLOT_USAGES;
import static org.ax1.lisp.stubs.index.LispVariableDefinitionIndex.VARIABLE_DEFINITIONS;
import static org.ax1.lisp.stubs.index.LispVariableUsageIndex.VARIABLE_USAGES;

@Service
public final class ProjectData {

  private final Project project;

  public ProjectData(Project project) {
    this.project = project;
  }

  public static ProjectData getInstance(Project project) {
    return project.getService(ProjectData.class);
  }

  public Collection<String> getAllFunctionDefinitionNames() {
    ArrayList<String> res = new ArrayList<>();
    StubIndex index = StubIndex.getInstance();
    index.processAllKeys(FUNCTION_DEFINITIONS, project, key -> {
      res.add(key);
      return true;
    });
    res.addAll(CommonLispPackage.FUNCTIONS);
    return res;
  }

  public Collection<String> getAllVariableDefinitionNames() {
    // TODO: add names from common lisp standard libraries, and other libraries.
    ArrayList<String> res = new ArrayList<>();
    StubIndex index = StubIndex.getInstance();
    index.processAllKeys(VARIABLE_DEFINITIONS, project, key -> {
      res.add(key);
      return true;
    });
    res.addAll(CommonLispPackage.VARIABLES);
    return res;
  }

  public boolean functionExists(LispStringDesignator name) {
    return getFunctionDefinition(name) != null || CommonLispPackage.FUNCTIONS.contains(name.getLispName());
  }

  public boolean variableExists(LispStringDesignator name) {
    return getVariableDefinition(name) != null || CommonLispPackage.VARIABLES.contains(name.getLispName());
  }

  public LispStringDesignator getFunctionDefinition(LispStringDesignator name) {
    return getSymbol(FUNCTION_DEFINITIONS, name);
  }

  public LispStringDesignator getVariableDefinition(LispStringDesignator name) {
    return getSymbol(VARIABLE_DEFINITIONS, name);
  }

  public @Nullable LispStringDesignator getClassDefinition(LispStringDesignator name) {
    return getSymbol(CLASS_DEFINITIONS, name);
  }

  public @Nullable LispStringDesignator getSlotDefinition(LispStringDesignator name) {
    return getSymbol(SLOT_DEFINITIONS, name);
  }

  public @Nullable LispStringDesignator getPackageDefinition(String name) {
    return getSingleIndexValue(PACKAGE_DEFINITIONS, name);
  }

  public Package getPackage(String name) {
    if (CommonLispUserPackage.INSTANCE.is(name)) {
      return CommonLispUserPackage.INSTANCE;
    }
    if (CommonLispPackage.INSTANCE.is(name)) {
      return CommonLispPackage.INSTANCE;
    }
    LispStringDesignator packageDesignator = getPackageDefinition(name);
    if (packageDesignator == null) return null;
    return packageDesignator.getPackageDefinition();
  }

  @Nullable
  private LispStringDesignator getSingleIndexValue(StubIndexKey<String, LispStringDesignator> functionDefinitions, String name) {
    Collection<LispStringDesignator> results = StubIndex.getElements(functionDefinitions, name, project, null, LispStringDesignator.class);
    if (results.isEmpty()) return null;
    return results.iterator().next();
  }

  public Collection<LispStringDesignator> getMethodDefinitions(LispStringDesignator name) {
    return getSymbols(METHOD_DEFINITIONS, name);
  }

  public Collection<LispStringDesignator> getFunctionUsages(LispStringDesignator name) {
    return getSymbols(FUNCTION_USAGES, name);
  }

  public Collection<LispStringDesignator> getVariableUsages(LispStringDesignator name) {
    return getSymbols(VARIABLE_USAGES, name);
  }

  public Collection<LispStringDesignator> getClassUsages(LispStringDesignator name) {
    return getSymbols(CLASS_USAGES, name);
  }

  public Collection<LispStringDesignator> getSlotUsages(LispStringDesignator name) {
    return getSymbols(SLOT_USAGES, name);
  }

  public Collection<LispStringDesignator> getPackageUsages(String name) {
    return StubIndex.getElements(PACKAGE_USAGES, name, project, null, LispStringDesignator.class);
  }

  private LispStringDesignator getSymbol(StubIndexKey<String, LispStringDesignator> indexKey, LispStringDesignator name) {
    Collection<LispStringDesignator> symbols = getSymbols(indexKey, name);
    if (symbols.isEmpty()) return null;
    return symbols.iterator().next();
  }

  private Collection<LispStringDesignator> getSymbols(StubIndexKey<String, LispStringDesignator> indexKey, LispStringDesignator name) {
    return StubIndex.getElements(indexKey, name.getLispName(), project, null, LispStringDesignator.class).stream()
        .filter(it -> it.getPackageName().equals(name.getPackageName())).collect(Collectors.toUnmodifiableList());
  }
}
