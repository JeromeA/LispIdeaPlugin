package org.ax1.lisp.psi.impl;

import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import org.ax1.lisp.psi.*;
import org.ax1.lisp.subprocess.SubprocessFeatures;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public abstract class LispListMixinImpl extends ASTWrapperPsiElement implements LispList {

  public LispListMixinImpl(@NotNull ASTNode node) {
    super(node);
  }

  @NotNull
  public List<LispSexp> getSexpList() {
    return SubprocessFeatures.getInstance(getProject()).filterOptionalSexpList(getPrefixedSexpList());
  }
}
