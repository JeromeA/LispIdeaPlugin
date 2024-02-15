package org.ax1.lisp.codeinspection;

import com.intellij.codeInspection.LocalInspectionTool;
import com.intellij.codeInspection.LocalQuickFix;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import org.ax1.lisp.psi.LispElementFactory;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispVisitor;
import org.jetbrains.annotations.NotNull;

public class PlusOneInspection extends LocalInspectionTool {

  @NotNull
  @Override
  public PsiElementVisitor buildVisitor(@NotNull ProblemsHolder holder, boolean isOnTheFly) {
    return new LispVisitor() {
      @Override
      public void visitList(@NotNull LispList list) {
        PsiElement[] children = list.getChildren();
        if (children.length == 3 && children[0].getText().equals("+")
            && (children[1].getText().equals("1") || children[2].getText().equals("1"))) {
          holder.registerProblem(list, "Use '+1' to add 1", new PlusOneQuickFix());
        }
      }
    };
  }

  private static class PlusOneQuickFix implements LocalQuickFix {

    @NotNull
    @Override
    public String getName() {
      return "Replace '+' with '1+'";
    }

    @NotNull
    @Override
    public String getFamilyName() {
      return getName();
    }

    @Override
    public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor descriptor) {
      PsiElement list = descriptor.getPsiElement();
      PsiElement[] children = list.getChildren();
      String arg;
      if (children[1].getText().equals("1")) {
        arg = children[2].getText();
      } else {
        arg = children[1].getText();
      }
      list.getParent().replace(LispElementFactory.createSexp(project, "(1+ " + arg + ")").getSexp());
    }
  }


}
