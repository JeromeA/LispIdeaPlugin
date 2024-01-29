package org.ax1.lisp.analysis.symbol;


import com.intellij.openapi.project.Project;
import org.ax1.lisp.analysis.ProjectData;
import org.ax1.lisp.psi.impl.LispStringDesignator;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class Package {

  public final String name;
  public String description;
  public final Set<String> nicknames = new HashSet<>();
  public final Set<String> uses = new HashSet<>();
  public final Set<String> exports = new HashSet<>();
  public final Set<String> shadows = new HashSet<>();
  // Warning: this is a symbol to package map, as this is the way we need to query it.
  public final Map<String, String> importFrom = new HashMap<>();

  public Package(String name) {
    this.name = name;
  }

  public void addUse(String use) {
    uses.add(use);
  }

  public void addNickname(String nickname) {
    nicknames.add(nickname);
  }

  public void addExport(String symbolName) {
    exports.add(symbolName);
  }

  public void addImportFrom(String symbolName, String packageName) {
    importFrom.put(symbolName, packageName);
  }

  public boolean is(String packageName) {
    if (name.equals(packageName)) return true;
    return nicknames.stream().anyMatch(nickName -> nickName.equals(packageName));
  }

  public String resolvePackage(String designator, Project project) {
    // Find if a used package exports the symbol.
    ProjectData projectData = ProjectData.getInstance(project);
    for (String use : uses) {
      Package packageDefinition = projectData.getPackage(use);
      if (packageDefinition == null) continue;
      if (packageDefinition.exports.contains(designator)) {
        return packageDefinition.name;
      }
    }
    return name;
  }
}
