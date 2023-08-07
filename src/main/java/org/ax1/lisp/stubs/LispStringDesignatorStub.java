package org.ax1.lisp.stubs;

import com.intellij.psi.PsiElement;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.StubBase;
import com.intellij.psi.stubs.StubElement;
import org.ax1.lisp.analysis.BaseLispElement;
import org.jetbrains.annotations.Nullable;

public class LispStringDesignatorStub<T extends PsiElement> extends StubBase<T> implements LispStringDesignatorStubInterface {

  private final String stringValue;
  private final BaseLispElement.Type type;

  protected LispStringDesignatorStub(@Nullable StubElement parent, IStubElementType elementType, String stringValue, BaseLispElement.Type type) {
    super(parent, elementType);
    this.stringValue = stringValue;
    this.type = type;
  }

  public String getStringValue() {
    return stringValue;
  }

  public BaseLispElement.Type getType() {
    return type;
  }
}
