package org.ax1.lisp.psi.impl;

import com.intellij.psi.PsiNameIdentifierOwner;
import org.ax1.lisp.analysis.symbol.PackageDefinition;

public interface LispSexpMixin extends PsiNameIdentifierOwner {

  PackageDefinition getPackageDefinition();
}
