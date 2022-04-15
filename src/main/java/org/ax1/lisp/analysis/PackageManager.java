package org.ax1.lisp.analysis;

import com.intellij.openapi.components.Service;
import com.intellij.openapi.project.Project;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

@Service(Service.Level.PROJECT)
public final class PackageManager {

  private final Map<String, Package> packages = new HashMap<>();

  public static PackageManager getInstance(Project project) {
    return project.getService(PackageManager.class);
  }

  public PackageManager() {
    Package commonLisp = new CommonLispPackage();
    add(commonLisp);
    Package commonLispUser = new Package(Set.of("common-lisp-user", "cl-user"), Set.of(commonLisp));
    add(commonLispUser);
  }

  public Package get(String name) {
    return packages.get(name);
  }

  public void add(Package packageToAdd) {
    packageToAdd.getNames().forEach(name -> packages.put(name, packageToAdd));
  }
}
