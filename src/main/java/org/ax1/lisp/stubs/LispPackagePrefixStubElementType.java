package org.ax1.lisp.stubs;

import com.intellij.psi.stubs.*;
import org.ax1.lisp.LispLanguage;
import org.ax1.lisp.analysis.BaseLispElement;
import org.ax1.lisp.psi.LispPackagePrefix;
import org.ax1.lisp.psi.impl.LispPackagePrefixImpl;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

public class LispPackagePrefixStubElementType extends LispStubElementType<LispPackagePrefixStub, LispPackagePrefix> {
  public static final LispPackagePrefixStubElementType INSTANCE = new LispPackagePrefixStubElementType();

  public LispPackagePrefixStubElementType() {
    super("lisp.PackagePrefix", LispLanguage.INSTANCE);
  }

  @Override
  public LispPackagePrefix createPsi(@NotNull LispPackagePrefixStub stub) {
    return new LispPackagePrefixImpl(stub, INSTANCE);
  }

  @Override
  protected LispPackagePrefixStub createStub(StubElement parentStub, String stringValue, BaseLispElement.Type type) {
    return new LispPackagePrefixStub(parentStub, INSTANCE, stringValue, type);
  }
}
