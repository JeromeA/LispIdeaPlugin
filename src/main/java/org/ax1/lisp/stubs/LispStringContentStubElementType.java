package org.ax1.lisp.stubs;

import com.intellij.psi.PsiElement;
import com.intellij.psi.stubs.*;
import org.ax1.lisp.LispLanguage;
import org.ax1.lisp.psi.LispStringContent;
import org.ax1.lisp.psi.impl.LispStringContentImpl;
import org.ax1.lisp.stubs.index.LispFunctionDefinitionIndex;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

public class LispStringContentStubElementType extends IStubElementType<LispStringContentStub, LispStringContent> {
  public static final LispStringContentStubElementType INSTANCE = new LispStringContentStubElementType();

  public LispStringContentStubElementType() {
    super("lisp.StringContent", LispLanguage.INSTANCE);
  }

  @Override
  public LispStringContent createPsi(@NotNull LispStringContentStub stub) {
    return new LispStringContentImpl(stub, INSTANCE);
  }

  @Override
  public @NotNull LispStringContentStub createStub(@NotNull LispStringContent psi, StubElement<? extends PsiElement> parentStub) {
    return new LispStringContentStub(parentStub, INSTANCE, psi.getValue());
  }

  @Override
  public @NotNull String getExternalId() {
    return toString();
  }

  @Override
  public void serialize(@NotNull LispStringContentStub stub, @NotNull StubOutputStream dataStream) throws IOException {
    dataStream.writeName(stub.getStringValue());
  }

  @Override
  public @NotNull LispStringContentStub deserialize(@NotNull StubInputStream dataStream, StubElement parentStub) throws IOException {
    return new LispStringContentStub(parentStub, INSTANCE, dataStream.readName().getString());
  }

  @Override
  public void indexStub(@NotNull LispStringContentStub stub, @NotNull IndexSink sink) {
    sink.occurrence(LispFunctionDefinitionIndex.FUNCTION_DEFINITIONS, stub.getStringValue());
  }
}
