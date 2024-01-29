package org.ax1.lisp.stubs;

import com.intellij.psi.PsiElement;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.StubBase;
import com.intellij.psi.stubs.StubElement;
import org.ax1.lisp.analysis.BaseLispElement;
import org.jetbrains.annotations.Nullable;

public class LispStringDesignatorStub<T extends PsiElement> extends StubBase<T> implements LispStringDesignatorStubInterface {

  private final String packageContext;
  private final String lispName;
  private final BaseLispElement.Type type;

  protected LispStringDesignatorStub(@Nullable StubElement parent, IStubElementType elementType, String packageContext, String lispName, BaseLispElement.Type type) {
    super(parent, elementType);
    this.packageContext = packageContext;
    this.lispName = lispName;
    this.type = type;
  }

  public String getPackageContext() {
    return packageContext;
  }

  public String getLispName() {
    return lispName;
  }

  public BaseLispElement.Type getType() {
    return type;
  }
}
