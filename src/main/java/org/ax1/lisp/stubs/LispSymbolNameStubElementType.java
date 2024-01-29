package org.ax1.lisp.stubs;

import com.intellij.psi.stubs.*;
import org.ax1.lisp.LispLanguage;
import org.ax1.lisp.analysis.BaseLispElement;
import org.ax1.lisp.psi.LispSymbolName;
import org.ax1.lisp.psi.impl.LispSymbolNameImpl;
import org.jetbrains.annotations.NotNull;

public class LispSymbolNameStubElementType extends LispStubElementType<LispSymbolNameStub, LispSymbolName> {
  public static final LispSymbolNameStubElementType INSTANCE = new LispSymbolNameStubElementType();

  public LispSymbolNameStubElementType() {
    super("lisp.symbolName", LispLanguage.INSTANCE);
  }

  @Override
  public LispSymbolName createPsi(@NotNull LispSymbolNameStub stub) {
    return new LispSymbolNameImpl(stub, INSTANCE);
  }

  @Override
  protected LispSymbolNameStub createStub(StubElement parentStub, String packageContext, String symbolName, BaseLispElement.Type type) {
    return new LispSymbolNameStub(parentStub, INSTANCE, packageContext, symbolName, type);
  }
}
