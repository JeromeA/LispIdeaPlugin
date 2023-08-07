package org.ax1.lisp.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.tree.IElementType;
import org.ax1.lisp.psi.LispPackagePrefix;
import org.ax1.lisp.stubs.LispPackagePrefixStub;
import org.ax1.lisp.stubs.LispStringDesignatorStubBase;
import org.jetbrains.annotations.NotNull;

public abstract class LispPackagePrefixMixinImpl
    extends LispStringDesignatorStubBase<LispPackagePrefixStub>
    implements LispPackagePrefix {

  public LispPackagePrefixMixinImpl(@NotNull LispPackagePrefixStub stub, @NotNull IStubElementType<?, ?> nodeType) {
    super(stub, nodeType);
    setType(Type.PACKAGE_USAGE);
  }

  public LispPackagePrefixMixinImpl(@NotNull ASTNode node) {
    super(node);
    setType(Type.PACKAGE_USAGE);
  }

  public LispPackagePrefixMixinImpl(LispPackagePrefixStub stub, IElementType nodeType, ASTNode node) {
    super(stub, nodeType, node);
    setType(Type.PACKAGE_USAGE);
  }
}
