package org.ax1.lisp.stubs;

import com.intellij.psi.PsiElement;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.StubBase;
import com.intellij.psi.stubs.StubElement;
import org.ax1.lisp.psi.LispElementFactory;
import org.ax1.lisp.psi.LispStringContent;
import org.jetbrains.annotations.Nullable;

public class LispStringDesignatorStub<T extends PsiElement> extends StubBase<T> {

  private String stringValue;

  protected LispStringDesignatorStub(@Nullable StubElement parent, IStubElementType elementType, String stringValue) {
    super(parent, elementType);
    this.stringValue = stringValue;
  }

  public String getStringValue() {
    return stringValue;
  }



}
