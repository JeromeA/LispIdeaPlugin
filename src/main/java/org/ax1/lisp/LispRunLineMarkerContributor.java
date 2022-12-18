package org.ax1.lisp;

import com.intellij.execution.lineMarker.RunLineMarkerContributor;
import com.intellij.icons.AllIcons;
import com.intellij.openapi.actionSystem.ActionManager;
import com.intellij.openapi.project.DumbAware;
import com.intellij.psi.PsiElement;
import com.intellij.psi.impl.source.tree.LeafPsiElement;
import org.ax1.lisp.psi.LispFile;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.subprocess.LispRunExpressionAction;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

import static org.ax1.lisp.psi.LispTypes.LPAREN;

public class LispRunLineMarkerContributor extends RunLineMarkerContributor implements DumbAware {

  @Override
  public @Nullable Info getInfo(@NotNull PsiElement element) {
    if (!isTopLevel(element)) return null;
    return new Info(AllIcons.RunConfigurations.TestState.Run,
        this::getToolTip,
        ActionManager.getInstance().getAction(LispRunExpressionAction.ID));
  }

  private String getToolTip(PsiElement element) {
    String name = getFormName(element);
    if (name == null) return "Evaluate form";
    return "Evaluate " + name + " form";
  }

  private String getFormName(PsiElement element) {
    LispList list = (LispList) element.getParent();
    List<LispSexp> sexpList = list.getSexpList();
    if (sexpList.isEmpty()) return null;
    LispSexp formName = sexpList.get(0);
    if (!formName.isSymbol()) return null;
    return formName.getText();
  }

  private boolean isTopLevel(PsiElement element) {
    if (!(element instanceof LeafPsiElement)) return false;
    LeafPsiElement leafPsiElement = (LeafPsiElement) element;
    if (leafPsiElement.getElementType() != LPAREN) return false;
    // - parent 1 is a List.
    // - parent 2 is a Sexp.
    // - parent 3 is a ListFile.
    return element.getParent().getParent().getParent() instanceof LispFile;
  }
}
