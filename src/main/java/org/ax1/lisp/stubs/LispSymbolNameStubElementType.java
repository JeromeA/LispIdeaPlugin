package org.ax1.lisp.stubs;

import com.intellij.psi.PsiElement;
import com.intellij.psi.stubs.*;
import org.ax1.lisp.LispLanguage;
import org.ax1.lisp.psi.LispSymbolName;
import org.ax1.lisp.psi.impl.LispSymbolNameImpl;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

public class LispSymbolNameStubElementType extends IStubElementType<LispSymbolNameStub, LispSymbolName> {
  public static final LispSymbolNameStubElementType INSTANCE = new LispSymbolNameStubElementType();

  public LispSymbolNameStubElementType() {
    super("lisp.symbolName", LispLanguage.INSTANCE);
  }

  @Override
  public LispSymbolName createPsi(@NotNull LispSymbolNameStub stub) {
    return new LispSymbolNameImpl(stub, INSTANCE);
  }

  @Override
  public @NotNull LispSymbolNameStub createStub(@NotNull LispSymbolName psi, StubElement<? extends PsiElement> parentStub) {
    return new LispSymbolNameStub(parentStub, INSTANCE, psi.getValue());
  }

  @Override
  public @NotNull String getExternalId() {
    return toString();
  }

  @Override
  public void serialize(@NotNull LispSymbolNameStub stub, @NotNull StubOutputStream dataStream) throws IOException {
    dataStream.writeName(stub.getStringValue());
  }

  @Override
  public @NotNull LispSymbolNameStub deserialize(@NotNull StubInputStream dataStream, StubElement parentStub) throws IOException {
    return new LispSymbolNameStub(parentStub, INSTANCE, dataStream.readName().getString());
  }

  @Override
  public void indexStub(@NotNull LispSymbolNameStub stub, @NotNull IndexSink sink) {
    System.err.println("Indexing stub "  + stub);
    System.err.println("  -> "  + stub.getStringValue());
    sink.occurrence(LispNameIndex.LISP_NAMES, stub.getStringValue());
  }
}
