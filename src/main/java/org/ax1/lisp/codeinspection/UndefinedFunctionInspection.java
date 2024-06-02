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

import java.util.ArrayList;
import java.util.List;

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
      List<String> parameters = new ArrayList<>();
      LispList list = (LispList) symbolName.getParent().getParent().getParent().getParent();
      list.getSexpList().stream().skip(1)
          .forEach(sexp -> parameters.add(argToParameterName(parameters, sexp)));
      LispPrefixedSexp function = createSexp(project,
          "(defun " + symbolName.getText()
              + " (" + String.join(" ", parameters) + ")\n"
              + "    (error \"Not implemented\"))");
      // Add the function.
      LispPrefixedSexp topLevelContainer = getTopLevel(symbolName);
      topLevelContainer.getParent().addAfter(function, topLevelContainer);
      // Add an empty line.
      topLevelContainer.getParent().addAfter(createNewline(project), topLevelContainer);
      topLevelContainer.getParent().addAfter(createNewline(project), topLevelContainer);
    }

    private static String argToParameterName(List<String> parameters, LispSexp sexp) {
      if (sexp.getSymbolName() == null) return "param" + parameters.size();
      String name = sexp.getSymbolName().getText();
      if (name.startsWith("get-") && name.length() > 4) return name.substring(4);
      if (name.startsWith("get") && name.length() > 3) return name.substring(3);
      return name;
    }

    private LispPrefixedSexp getTopLevel(PsiElement psiElement) {
      while (!(psiElement.getParent() instanceof LispFile)) {
        psiElement = psiElement.getParent();
      }
      return (LispPrefixedSexp) psiElement;
    }
  }


}
