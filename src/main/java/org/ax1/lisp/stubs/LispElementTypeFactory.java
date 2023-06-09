package org.ax1.lisp.stubs;

import com.intellij.psi.stubs.IStubElementType;

public class LispElementTypeFactory {

  public static IStubElementType stubFactory(String name) {
    if (name.equals("SYMBOL_NAME")) return LispSymbolNameStubElementType.INSTANCE;
    if (name.equals("PACKAGE_PREFIX")) return LispPackagePrefixStubElementType.INSTANCE;
    if (name.equals("STRING_CONTENT")) return LispStringContentStubElementType.INSTANCE;
    throw new RuntimeException("Unknown element type: " + name);
  }

}
