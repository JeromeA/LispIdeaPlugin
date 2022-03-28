package org.ax1.lisp.psi;

import com.intellij.psi.tree.IElementType;
import org.ax1.lisp.LispLanguage;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

public class LispTokenType extends IElementType {

  public LispTokenType(@NotNull @NonNls String debugName) {
    super(debugName, LispLanguage.INSTANCE);
  }

  @Override
  public String toString() {
    return "LispTokenType." + super.toString();
  }
}
