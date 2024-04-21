package org.ax1.lisp.subprocess;

import com.intellij.execution.Location;
import com.intellij.execution.PsiLocation;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.project.DumbAwareAction;
import com.intellij.openapi.ui.Messages;
import com.intellij.psi.PsiElement;
import com.intellij.psi.impl.source.tree.LeafPsiElement;
import com.intellij.psi.tree.IElementType;
import org.ax1.lisp.psi.*;
import org.jetbrains.annotations.NotNull;

import java.util.List;

import static org.ax1.lisp.psi.LispTypes.*;
import static org.ax1.lisp.psi.LispTypes.LPAREN;

public class LispRunExpressionAction extends DumbAwareAction {
  public static final String ID = "LispRunExpressionAction";

  @Override
  public void actionPerformed(@NotNull AnActionEvent e) {
    PsiElement topLevelExpression = getTopLevelExpression(getPsiElement(e));
    if (!(topLevelExpression instanceof LispPrefixedSexp)) {
      Messages.showErrorDialog(e.getProject(), "The top level expression is not a Sexp", "Error");
      return;
    }
    LispSexp topLevelSexp = ((LispPrefixedSexp) topLevelExpression).getSexp();
    LispServer lispServer = LispServer.getInstance(e.getProject());
    lispServer.evaluate(getInPackageExpression(topLevelSexp.getPackageContext()), false);
    lispServer.evaluate(topLevelExpression.getText(), true);
  }

  private String getInPackageExpression(String packageName) {
    if (packageName == null) {
      return null;
    }
    return "(IN-PACKAGE \"" + packageName + "\")";
  }

  @NotNull
  private static PsiElement getTopLevelExpression(PsiElement element) {
    while (! (element.getParent() instanceof LispFile)) {
      element = element.getParent();
    }
    return element;
  }

  private static PsiElement getPsiElement(@NotNull AnActionEvent e) {
    PsiLocation<PsiElement> location = (PsiLocation<PsiElement>) e.getData(Location.DATA_KEY);
    if (location == null) return null;
    return location.getPsiElement();
  }

  public static String getEvalDescription(PsiElement element) {
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
    return "Invalid Evaluate form";
  }

  @Override
  public void update(@NotNull AnActionEvent actionEvent) {
    PsiElement psiElement = getPsiElement(actionEvent);
    if (psiElement != null) {
      actionEvent.getPresentation().setText(getEvalDescription(psiElement));
    }
  }
}
