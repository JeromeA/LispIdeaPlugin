package org.ax1.lisp.stubs;

import com.intellij.lang.Language;
import com.intellij.psi.PsiElement;
import com.intellij.psi.stubs.*;
import com.intellij.util.io.StringRef;
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

  private static final Map<BaseLispElement.Type, StubIndexKey<String, LispStringDesignator>> INDEX = Map.of(
      BaseLispElement.Type.FUNCTION_DEFINITION, LispFunctionDefinitionIndex.FUNCTION_DEFINITIONS,
      BaseLispElement.Type.FUNCTION_USAGE, LispFunctionUsageIndex.FUNCTION_USAGES,
      BaseLispElement.Type.CLASS_DEFINITION, LispClassDefinitionIndex.CLASS_DEFINITIONS,
      BaseLispElement.Type.CLASS_USAGE, LispClassUsageIndex.CLASS_USAGES,
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
    dataStream.writeName(stub.getPackageContext());
    dataStream.writeName(stub.getLispName());
    dataStream.writeChar(stub.getType().ordinal());
  }

  @Override
  public @NotNull A deserialize(@NotNull StubInputStream dataStream, StubElement parentStub) throws IOException {
    String packageContext = readString(dataStream);
    String lispName = readString(dataStream);
    BaseLispElement.Type type = BaseLispElement.Type.values()[dataStream.readChar()];
    return createStub(parentStub, packageContext, lispName, type);
  }

  private static String readString(@NotNull StubInputStream dataStream) throws IOException {
    StringRef stringRef = dataStream.readName();
    if (stringRef == null) return null;
    return stringRef.getString();
  }

  @Override
  public @NotNull A createStub(@NotNull B psi, StubElement<? extends PsiElement> parentStub) {
    return createStub(parentStub, psi.getPackageContext(), psi.getLispName(), psi.getType());
  }

  protected abstract A createStub(StubElement parentStub, String packagePrefix, String lispName, BaseLispElement.Type type);

  @Override
  public @NotNull String getExternalId() {
    return toString();
  }

  @Override
  public void indexStub(@NotNull A stub, @NotNull IndexSink sink) {
    StubIndexKey<String, LispStringDesignator> indexKey = INDEX.get(stub.getType());
    if (indexKey != null) {
//      System.err.println("Indexing "  + stub.getLispName() + " as " + stub.getType());
      sink.occurrence(indexKey, stub.getLispName());
    }
    if (stub.getType() == BaseLispElement.Type.SYMBOL_USAGE) {
      sink.occurrence(LispFunctionUsageIndex.FUNCTION_USAGES, stub.getLispName());
      sink.occurrence(LispVariableUsageIndex.VARIABLE_USAGES, stub.getLispName());
      sink.occurrence(LispClassUsageIndex.CLASS_USAGES, stub.getLispName());
    }
  }
}
