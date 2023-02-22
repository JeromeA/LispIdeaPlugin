package org.ax1.lisp;

import com.intellij.execution.lineMarker.RunLineMarkerContributor;
import com.intellij.icons.AllIcons;
import com.intellij.openapi.actionSystem.ActionManager;
import com.intellij.openapi.project.DumbAware;
import com.intellij.psi.PsiElement;
import com.intellij.psi.impl.source.tree.LeafPsiElement;
import com.intellij.psi.tree.IElementType;
import org.ax1.lisp.psi.*;
import org.ax1.lisp.subprocess.LispRunExpressionAction;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

import static org.ax1.lisp.psi.LispTypes.*;

public class LispRunLineMarkerContributor extends RunLineMarkerContributor implements DumbAware {

  @Override
  public @Nullable Info getInfo(@NotNull PsiElement element) {
    if (!isTopLevel(element)) return null;
    return new Info(AllIcons.RunConfigurations.TestState.Run,
        this::getToolTip,
        ActionManager.getInstance().getAction(LispRunExpressionAction.ID));
  }

  private String getToolTip(PsiElement element) {
    LeafPsiElement leafPsiElement = (LeafPsiElement) element;
    IElementType elementType = leafPsiElement.getElementType();
    if (elementType == SYMBOL_TOKEN) {
      return "Evaluate symbol " + element.getText();
    }
    if (elementType == NUMBER) {
      return "Evaluate number " + element.getText();
    }
    if (elementType == CHARACTER) {
      return "Evaluate character " + element.getText();
    }
    if (elementType == STRING_QUOTE) {
      return "Evaluate string " + element.getParent().getText();
    }
    if (elementType == QUOTE) {
      return "Evaluate QUOTE form";
    }
    if (elementType == LPAREN) {
      LispList list = (LispList) element.getParent();
      List<LispSexp> sexpList = list.getSexpList();
      if (sexpList.isEmpty()) return "Evaluate NIL";
      LispSexp sexp0 = sexpList.get(0);
      LispSymbol symbol0 = sexp0.getSymbol();
      if (symbol0 != null) return "Evaluate " + symbol0.getText() + " form";
      return "Evaluate form";
    }
    return "Invalid form";
  }

  /**
   * The options are:
   * Sexp -> Symbol -> SymbolName -> SYMBOL_TOKEN
   * Sexp -> List -> LPAREN
   * Sexp -> Quoted -> QUOTE
   * Sexp -> String -> STRING_QUOTE
   * Sexp -> NUMBER | CHARACTER
   */
  private boolean isTopLevel(PsiElement element) {
    if (!(element instanceof LeafPsiElement)) return false;
    LeafPsiElement leafPsiElement = (LeafPsiElement) element;
    IElementType elementType = leafPsiElement.getElementType();
    if (elementType == SYMBOL_TOKEN) {
      return element.getParent().getParent().getParent().getParent() instanceof LispFile;
    }
    if (elementType == LPAREN || elementType == QUOTE) {
      return element.getParent().getParent().getParent() instanceof LispFile;
    }
    if (elementType == STRING_QUOTE) {
      // When opening metering.lisp in slime, the parent is a weird dummy object.
      if (!(element.getParent() instanceof LispString)) return false;
      LispString string = (LispString) element.getParent();
      return string.getFirstChild() == element && string.getParent().getParent() instanceof LispFile;
    }
    if (elementType == NUMBER || elementType == CHARACTER) {
      return element.getParent().getParent() instanceof LispFile;
    }
    return false;
  }
}
