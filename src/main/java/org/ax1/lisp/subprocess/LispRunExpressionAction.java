package org.ax1.lisp.subprocess;

import com.intellij.execution.Location;
import com.intellij.execution.PsiLocation;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.project.DumbAwareAction;
import com.intellij.psi.PsiElement;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public class LispRunExpressionAction extends DumbAwareAction {
  public static final String ID = "LispRunExpressionAction";

  @Override
  public void actionPerformed(@NotNull AnActionEvent e) {
    System.err.println("Evaluating Lisp Expression...");
    LispConsole.getInstance(e.getProject()).evaluate(getLispList(e).getText());
  }

  @Nullable
  private static LispList getLispList(@NotNull AnActionEvent e) {
    PsiLocation<PsiElement> location = (PsiLocation) e.getDataContext().getData(Location.DATA_KEY);
    if (location == null) return null;
    PsiElement lparen = location.getPsiElement();
    return (LispList) lparen.getParent();
  }

  @Override
  public void update(@NotNull AnActionEvent e) {
    String functionName = getFunctionName(e);
    e.getPresentation().setText(
        functionName == null ? "Evaluate Expression" : String.format("Evaluate %S Expression", functionName));
  }

  private static String getFunctionName(@NotNull AnActionEvent e) {
    LispList list = getLispList(e);
    if (list == null) return null;
    List<LispSexp> sexpList = list.getSexpList();
    if (sexpList.isEmpty()) return null;
    return sexpList.get(0).getText();
  }
}
