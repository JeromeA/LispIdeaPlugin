package org.ax1.lisp.codeinspection;

import com.intellij.codeInspection.LocalInspectionTool;
import com.intellij.codeInspection.LocalQuickFix;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import org.ax1.lisp.analysis.BaseLispElement;
import org.ax1.lisp.analysis.ProjectData;
import org.ax1.lisp.psi.*;
import org.jetbrains.annotations.NotNull;

import static org.ax1.lisp.psi.LispElementFactory.createNewline;
import static org.ax1.lisp.psi.LispElementFactory.createSexp;

public class UndefinedFunctionInspection extends LocalInspectionTool {

  @NotNull
  @Override
  public PsiElementVisitor buildVisitor(@NotNull ProblemsHolder holder, boolean isOnTheFly) {
    return new LispVisitor() {
      @Override
      public void visitSymbolName(@NotNull LispSymbolName symbolName) {
        if (symbolName.getType() != BaseLispElement.Type.FUNCTION_USAGE) return;
        if (ProjectData.getInstance(symbolName.getProject()).functionExists(symbolName)) return;

        holder.registerProblem(symbolName, "Undefined function", new CreateFunctionQuickFix());
      }
    };
  }

  private static class CreateFunctionQuickFix implements LocalQuickFix {

    @NotNull
    @Override
    public String getName() {
      return "Create function";
    }

    @NotNull
    @Override
    public String getFamilyName() {
      return getName();
    }

    @Override
    public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor descriptor) {
      LispSymbolName symbolName = (LispSymbolName) descriptor.getPsiElement();
      LispPrefixedSexp topLevelContainer = getTopLevel(symbolName);
      LispPrefixedSexp function = createSexp(project, "(defun " + symbolName.getText() + " ())");
      // Add the function.
      topLevelContainer.getParent().addAfter(function, topLevelContainer);
      // Add an empty line.
      topLevelContainer.getParent().addAfter(createNewline(project), topLevelContainer);
      topLevelContainer.getParent().addAfter(createNewline(project), topLevelContainer);
    }

    private LispPrefixedSexp getTopLevel(PsiElement psiElement) {
      while (!(psiElement.getParent() instanceof LispFile)) {
        psiElement = psiElement.getParent();
      }
      return (LispPrefixedSexp) psiElement;
    }
  }


}
