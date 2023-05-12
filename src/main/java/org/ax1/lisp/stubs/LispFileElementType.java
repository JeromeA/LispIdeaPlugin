package org.ax1.lisp.stubs;

import com.intellij.psi.tree.IStubFileElementType;
import org.ax1.lisp.LispLanguage;

public class LispFileElementType extends IStubFileElementType<LispFileStub> {

  public static final IStubFileElementType INSTANCE = new LispFileElementType();
  public LispFileElementType() {
    super("LISP_FILE", LispLanguage.INSTANCE);
  }
}
