package org.ax1.lisp.analysis;

import com.intellij.openapi.components.Service;
import com.intellij.openapi.project.Project;
import com.intellij.psi.stubs.StubIndex;
import com.intellij.psi.stubs.StubIndexKey;
import org.ax1.lisp.psi.impl.LispStringDesignator;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collection;

import static org.ax1.lisp.stubs.index.LispFunctionDefinitionIndex.FUNCTION_DEFINITIONS;
import static org.ax1.lisp.stubs.index.LispFunctionUsageIndex.FUNCTION_USAGES;
import static org.ax1.lisp.stubs.index.LispMethodDefinitionIndex.METHOD_DEFINITIONS;
import static org.ax1.lisp.stubs.index.LispPackageDefinitionIndex.PACKAGE_DEFINITIONS;
import static org.ax1.lisp.stubs.index.LispPackageUsageIndex.PACKAGE_USAGES;
import static org.ax1.lisp.stubs.index.LispVariableDefinitionIndex.VARIABLE_DEFINITIONS;

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
    // TODO: add functions from common lisp standard libraries, and other libraries.
    ArrayList<String> res = new ArrayList<>();
    StubIndex index = StubIndex.getInstance();
    index.processAllKeys(FUNCTION_DEFINITIONS, project, key -> {
      res.add(key);
      return true;
    });
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
    return res;
  }

  public LispStringDesignator getFunctionDefinition(String name) {
    return getSingleIndexValue(FUNCTION_DEFINITIONS, name);
  }

  public LispStringDesignator getVariableDefinition(String name) {
    return getSingleIndexValue(VARIABLE_DEFINITIONS, name);
  }

  public LispStringDesignator getPackageDefinition(String name) {
    return getSingleIndexValue(PACKAGE_DEFINITIONS, name);
  }

  @Nullable
  private LispStringDesignator getSingleIndexValue(StubIndexKey<String, LispStringDesignator> functionDefinitions, String name) {
    Collection<LispStringDesignator> results = StubIndex.getInstance().get(functionDefinitions, name, project, null);
    if (results.isEmpty()) return null;
    return results.iterator().next();
  }

  public Collection<LispStringDesignator> getMethodDefinitions(String name) {
    return StubIndex.getInstance().get(METHOD_DEFINITIONS, name, project, null);
  }

  public Collection<LispStringDesignator> getFunctionUsages(String name) {
    return StubIndex.getInstance().get(FUNCTION_USAGES, name, project, null);
  }

  public Collection<LispStringDesignator> getPackageUsages(String name) {
    return StubIndex.getInstance().get(PACKAGE_USAGES, name, project, null);
  }
}
