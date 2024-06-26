package org.ax1.lisp.psi.impl;

import com.intellij.psi.PsiElement;
import org.ax1.lisp.analysis.BaseLispElement;
import org.ax1.lisp.psi.LispSexp;

import java.util.List;

public interface LispListMixin extends PsiElement, BaseLispElement {
  List<LispSexp> getSexpList();

}
