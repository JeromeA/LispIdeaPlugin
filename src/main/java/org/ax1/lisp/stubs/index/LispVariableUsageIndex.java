package org.ax1.lisp.stubs.index;

import com.intellij.psi.stubs.StringStubIndexExtension;
import com.intellij.psi.stubs.StubIndexKey;
import org.ax1.lisp.psi.impl.LispStringDesignator;
import org.jetbrains.annotations.NotNull;

public class LispVariableUsageIndex extends StringStubIndexExtension<LispStringDesignator> {
  public static final StubIndexKey<String, LispStringDesignator> VARIABLE_USAGES = StubIndexKey.createIndexKey("lisp.variable.usages");

  @Override
  public @NotNull StubIndexKey<String, LispStringDesignator> getKey() {
    return VARIABLE_USAGES;
  }
}
