package org.ax1.lisp.stubs.index;

import com.intellij.psi.stubs.StringStubIndexExtension;
import com.intellij.psi.stubs.StubIndexKey;
import org.ax1.lisp.psi.impl.LispStringDesignator;
import org.jetbrains.annotations.NotNull;

public class LispFunctionUsageIndex extends StringStubIndexExtension<LispStringDesignator> {
  public static final StubIndexKey<String, LispStringDesignator> FUNCTION_USAGES = StubIndexKey.createIndexKey("lisp.function.usages");

  @Override
  public @NotNull StubIndexKey<String, LispStringDesignator> getKey() {
    return FUNCTION_USAGES;
  }

}
