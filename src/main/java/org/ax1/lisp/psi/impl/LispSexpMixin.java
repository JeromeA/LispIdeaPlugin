package org.ax1.lisp.psi.impl;

import com.intellij.psi.PsiNamedElement;
import org.ax1.lisp.analysis.symbol.PackageDefinition;

public interface LispSexpMixin extends PsiNamedElement {

  PackageDefinition getPackageDefinition();
}
