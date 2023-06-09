package org.ax1.lisp.stubs;

import com.intellij.psi.PsiElement;
import com.intellij.psi.stubs.*;
import org.ax1.lisp.LispLanguage;
import org.ax1.lisp.psi.LispPackagePrefix;
import org.ax1.lisp.psi.impl.LispPackagePrefixImpl;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

public class LispPackagePrefixStubElementType extends IStubElementType<LispPackagePrefixStub, LispPackagePrefix> {
  public static final LispPackagePrefixStubElementType INSTANCE = new LispPackagePrefixStubElementType();

  public LispPackagePrefixStubElementType() {
    super("lisp.PackagePrefix", LispLanguage.INSTANCE);
  }

  @Override
  public LispPackagePrefix createPsi(@NotNull LispPackagePrefixStub stub) {
    return new LispPackagePrefixImpl(stub, INSTANCE);
  }

  @Override
  public @NotNull LispPackagePrefixStub createStub(@NotNull LispPackagePrefix psi, StubElement<? extends PsiElement> parentStub) {
    return new LispPackagePrefixStub(parentStub, INSTANCE, psi.getValue());
  }

  @Override
  public @NotNull String getExternalId() {
    return toString();
  }

  @Override
  public void serialize(@NotNull LispPackagePrefixStub stub, @NotNull StubOutputStream dataStream) throws IOException {
    dataStream.writeName(stub.getStringValue());
  }

  @Override
  public @NotNull LispPackagePrefixStub deserialize(@NotNull StubInputStream dataStream, StubElement parentStub) throws IOException {
    return new LispPackagePrefixStub(parentStub, INSTANCE, dataStream.readName().getString());
  }

  @Override
  public void indexStub(@NotNull LispPackagePrefixStub stub, @NotNull IndexSink sink) {
    sink.occurrence(LispNameIndex.LISP_NAMES, stub.getStringValue());
  }
}
