package org.ax1.lisp.stubs;

import com.intellij.psi.stubs.PsiFileStubImpl;
import org.ax1.lisp.psi.LispFile;

public class LispFileStub extends PsiFileStubImpl<LispFile> {
  public LispFileStub(LispFile file) {
    super(file);
  }
}
