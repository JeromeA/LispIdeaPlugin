package org.ax1.lisp.stubs;

import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.StubElement;
import org.ax1.lisp.analysis.BaseLispElement;
import org.ax1.lisp.psi.LispPackagePrefix;
import org.jetbrains.annotations.Nullable;

public class LispPackagePrefixStub extends LispStringDesignatorStub<LispPackagePrefix> {

  protected LispPackagePrefixStub(@Nullable StubElement parent, IStubElementType elementType, String packageName, BaseLispElement.Type type) {
    super(parent, elementType, packageName, type);
  }

}
