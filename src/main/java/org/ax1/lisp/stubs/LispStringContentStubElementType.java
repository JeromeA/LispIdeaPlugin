package org.ax1.lisp.stubs;

import com.intellij.psi.stubs.*;
import org.ax1.lisp.LispLanguage;
import org.ax1.lisp.analysis.BaseLispElement;
import org.ax1.lisp.psi.LispStringContent;
import org.ax1.lisp.psi.impl.LispStringContentImpl;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

public class LispStringContentStubElementType extends LispStubElementType<LispStringContentStub, LispStringContent> {
  public static final LispStringContentStubElementType INSTANCE = new LispStringContentStubElementType();

  public LispStringContentStubElementType() {
    super("lisp.StringContent", LispLanguage.INSTANCE);
  }

  @Override
  public LispStringContent createPsi(@NotNull LispStringContentStub stub) {
    return new LispStringContentImpl(stub, INSTANCE);
  }

  @Override
  protected LispStringContentStub createStub(StubElement parentStub, String packageContext, String lispName, BaseLispElement.Type type) {
    return new LispStringContentStub(parentStub, INSTANCE, packageContext, lispName, type);
  }
}
