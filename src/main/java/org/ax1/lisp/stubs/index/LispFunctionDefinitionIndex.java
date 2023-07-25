package org.ax1.lisp.stubs.index;

import com.intellij.psi.stubs.StringStubIndexExtension;
import com.intellij.psi.stubs.StubIndexKey;
import org.ax1.lisp.psi.impl.LispStringDesignator;
import org.jetbrains.annotations.NotNull;

public class LispFunctionDefinitionIndex extends StringStubIndexExtension<LispStringDesignator> {

  public static final StubIndexKey<String, LispStringDesignator> FUNCTION_DEFINITIONS = StubIndexKey.createIndexKey("lisp.function.definitions");

  @Override
  public @NotNull StubIndexKey<String, LispStringDesignator> getKey() {
    return FUNCTION_DEFINITIONS;
  }
}
