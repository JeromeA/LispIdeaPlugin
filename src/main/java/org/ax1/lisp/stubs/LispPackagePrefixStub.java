package org.ax1.lisp.stubs;

import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.StubElement;
import org.ax1.lisp.psi.LispPackagePrefix;
import org.jetbrains.annotations.Nullable;

public class LispPackagePrefixStub extends LispStringDesignatorStub<LispPackagePrefix> {

  protected LispPackagePrefixStub(@Nullable StubElement parent, IStubElementType elementType, String stringValue) {
    super(parent, elementType, stringValue);
  }

}
