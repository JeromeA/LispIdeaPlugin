package org.ax1.lisp.stubs;

import com.intellij.lang.Language;
import com.intellij.psi.PsiElement;
import com.intellij.psi.stubs.*;
import org.ax1.lisp.analysis.BaseLispElement;
import org.ax1.lisp.psi.impl.LispStringDesignator;
import org.ax1.lisp.stubs.index.*;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.util.Map;

public abstract class LispStubElementType<A extends StubElement<?> & LispStringDesignatorStubInterface, B extends PsiElement & LispStringDesignator>
    extends IStubElementType<A, B>  {

  private static Map<BaseLispElement.Type, StubIndexKey<String, LispStringDesignator>> index = Map.of(
      BaseLispElement.Type.FUNCTION_DEFINITION, LispFunctionDefinitionIndex.FUNCTION_DEFINITIONS,
      BaseLispElement.Type.FUNCTION_USAGE, LispFunctionUsageIndex.FUNCTION_USAGES,
      BaseLispElement.Type.PACKAGE_DEFINITION, LispPackageDefinitionIndex.PACKAGE_DEFINITIONS,
      BaseLispElement.Type.PACKAGE_USAGE, LispPackageUsageIndex.PACKAGE_USAGES,
      BaseLispElement.Type.VARIABLE_DEFINITION, LispVariableDefinitionIndex.VARIABLE_DEFINITIONS,
      BaseLispElement.Type.VARIABLE_USAGE, LispVariableUsageIndex.VARIABLE_USAGES
  );

  public LispStubElementType(@NotNull @NonNls String debugName, @Nullable Language language) {
    super(debugName, language);
  }

  @Override
  public void serialize(@NotNull A stub, @NotNull StubOutputStream dataStream) throws IOException {
    dataStream.writeName(stub.getStringValue());
    dataStream.writeChar(stub.getType().ordinal());
  }

  @Override
  public @NotNull A deserialize(@NotNull StubInputStream dataStream, StubElement parentStub) throws IOException {
    String stringValue = dataStream.readName().getString();
    BaseLispElement.Type type = BaseLispElement.Type.values()[dataStream.readChar()];
    return createStub(parentStub, stringValue, type);
  }

  @Override
  public @NotNull A createStub(@NotNull B psi, StubElement<? extends PsiElement> parentStub) {
    return createStub(parentStub, psi.getValue(), psi.getType());
  }

  protected abstract A createStub(StubElement parentStub, String stringValue, BaseLispElement.Type type);

  @Override
  public @NotNull String getExternalId() {
    return toString();
  }

  @Override
  public void indexStub(@NotNull A stub, @NotNull IndexSink sink) {
    StubIndexKey<String, LispStringDesignator> indexKey = index.get(stub.getType());
    if (indexKey != null) {
//      System.err.println("Indexing "  + stub.getStringValue() + " as " + stub.getType());
      sink.occurrence(indexKey, stub.getStringValue());
    }
  }
}
