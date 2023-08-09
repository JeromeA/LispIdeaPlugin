package org.ax1.lisp;

import com.intellij.openapi.components.Service;
import com.intellij.openapi.project.Project;
import com.intellij.psi.stubs.StubIndex;
import org.ax1.lisp.analysis.symbol.*;
import org.ax1.lisp.analysis.symbol.Package;
import org.ax1.lisp.psi.LispColon;
import org.ax1.lisp.psi.LispPackagePrefix;
import org.ax1.lisp.psi.LispSymbol;
import org.ax1.lisp.psi.LispSymbolName;
import org.ax1.lisp.psi.impl.LispStringDesignator;
import org.ax1.lisp.stubs.index.LispPackageDefinitionIndex;

import java.util.Collection;

import static com.intellij.openapi.components.Service.Level.PROJECT;

@Service(PROJECT)
public final class SymbolResolver {

  private final Project project;

  public static SymbolResolver getInstance(Project project) {
    return project.getService(SymbolResolver.class);
  }

  public SymbolResolver(Project project) {
    this.project = project;
  }

  public static Symbol resolve(LispSymbolName symbolName) {
    return getInstance(symbolName.getProject()).resolveImpl(symbolName);
  }

  private Symbol resolveImpl(LispSymbolName symbolName) {
    LispSymbol symbol = (LispSymbol) symbolName.getParent();
    LispPackagePrefix packagePrefix = symbol.getPackagePrefix();
    if (packagePrefix != null) {
      return new Symbol(packagePrefix.getText(), symbolName.getText());
    }
    LispColon colon = symbol.getColon();
    if (colon != null) return new Symbol("", symbolName.getLispName());
    return findSymbol(getPackageContext(symbolName), symbolName.getLispName());
  }

  private Package getPackageContext(LispSymbolName symbolName) {
    // TODO: find the closest IN-PACKAGE.
    // TODO: cache the instance in the project.
    return CommonLispUserPackage.INSTANCE;
  }

  private Package getPackageDefinition(String packageName) {
    if (CommonLispPackage.INSTANCE.is(packageName)) return CommonLispPackage.INSTANCE;
    if (CommonLispUserPackage.INSTANCE.is(packageName)) return CommonLispUserPackage.INSTANCE;
    Collection<LispStringDesignator> stringDesignators = StubIndex.getElements(LispPackageDefinitionIndex.PACKAGE_DEFINITIONS, packageName, project, null, LispStringDesignator.class);
    if (stringDesignators.isEmpty()) return null;
    LispStringDesignator stringDesignator = stringDesignators.iterator().next();
    return stringDesignator.getPackageDefinition();
  }

  public Symbol findSymbol(Package definition, String symbolName) {
    // IMPORT-FROM option.
    String importFromPackage = definition.importFrom.get(symbolName);
    if (importFromPackage != null) {
      Package importedPackage = getPackageDefinition(importFromPackage);
      if (importedPackage != null) {
        return findSymbol(importedPackage, symbolName);
      } else {
        return new Symbol(importFromPackage, symbolName);
      }
    }

    // USE option.
    for (String packageName : definition.uses) {
      Package usedPackage = getPackageDefinition(packageName);
      if (usedPackage != null) {
        Symbol importedSymbol = findExportedSymbol(usedPackage, symbolName);
        if (importedSymbol != null) {
          return importedSymbol;
        }
      }
    }

    return new Symbol(definition.name, symbolName);
  }

  private Symbol findExportedSymbol(Package definition, String symbolName) {
    if (!definition.exports.contains(symbolName)) {
      // We can't possibly be lenient and return a proper symbol in this case, because we need null to signal
      // that a symbol is not exported when looping over the USEs of a package.
      return null;
    }
    return findSymbol(definition, symbolName);
  }

}
