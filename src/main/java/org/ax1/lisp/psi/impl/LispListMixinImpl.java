package org.ax1.lisp.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.lang.annotation.AnnotationHolder;
import org.ax1.lisp.psi.*;
import org.ax1.lisp.subprocess.SubprocessFeatures;
import org.jetbrains.annotations.NotNull;

import java.util.List;

import static com.intellij.lang.annotation.HighlightSeverity.INFORMATION;
import static com.intellij.openapi.editor.DefaultLanguageHighlighterColors.CONSTANT;

public abstract class LispListMixinImpl extends BaseMixinImpl implements LispList {

  public LispListMixinImpl(@NotNull ASTNode node) {
    super(node);
  }

  @NotNull
  public List<LispSexp> getSexpList() {
    return SubprocessFeatures.getInstance(getProject()).filterOptionalSexpList(getPrefixedSexpList());
  }

  @Override
  public void setType(Type type) {
    // It's valid for a List to have a type, for (), for example, or to highlight the parentheses in DATA.
    super.setType(type);
    // Propagate ERROR. Propagating CODE would be a bug: a list needs to be CODE, but propagating it would prevent
    // the real types to be discovered.
    if (type == Type.ERROR) getSexpList().forEach(e -> e.setType(type));
  }

  @Override
  protected void annotateWithType(@NotNull AnnotationHolder holder) {
    if (getType() == Type.DATA) {
      holder.newSilentAnnotation(INFORMATION).range(getFirstChild()).textAttributes(CONSTANT).create();
      holder.newSilentAnnotation(INFORMATION).range(getLastChild()).textAttributes(CONSTANT).create();
    }
  }
}
