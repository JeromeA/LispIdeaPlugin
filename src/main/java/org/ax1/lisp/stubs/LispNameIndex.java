package org.ax1.lisp.stubs;

import com.intellij.psi.stubs.StringStubIndexExtension;
import com.intellij.psi.stubs.StubIndexKey;
import org.ax1.lisp.psi.impl.LispStringDesignator;
import org.jetbrains.annotations.NotNull;

public class LispNameIndex extends StringStubIndexExtension<LispStringDesignator> {

  public static final StubIndexKey<String, LispStringDesignator> LISP_NAMES = StubIndexKey.createIndexKey("lisp.name");

  @Override
  public @NotNull StubIndexKey<String, LispStringDesignator> getKey() {
    return LISP_NAMES;
  }
}
