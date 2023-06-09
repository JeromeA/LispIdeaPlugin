package org.ax1.lisp.stubs;

import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.StubElement;
import org.ax1.lisp.psi.LispStringContent;
import org.jetbrains.annotations.Nullable;

public class LispStringContentStub extends LispStringDesignatorStub<LispStringContent> {

  protected LispStringContentStub(@Nullable StubElement parent, IStubElementType elementType, String stringValue) {
    super(parent, elementType, stringValue);
  }

}
