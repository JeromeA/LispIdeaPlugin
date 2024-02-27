package org.ax1.lisp.stubs.index;

import com.intellij.psi.stubs.StringStubIndexExtension;
import com.intellij.psi.stubs.StubIndexKey;
import org.ax1.lisp.psi.impl.LispStringDesignator;
import org.jetbrains.annotations.NotNull;

public class LispClassDefinitionIndex extends StringStubIndexExtension<LispStringDesignator> {
  public static final StubIndexKey<String, LispStringDesignator> CLASS_DEFINITIONS = StubIndexKey.createIndexKey("lisp.class.definitions");

  @Override
  public @NotNull StubIndexKey<String, LispStringDesignator> getKey() {
    return CLASS_DEFINITIONS;
  }
}
