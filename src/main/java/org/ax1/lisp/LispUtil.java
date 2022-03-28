package org.ax1.lisp;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiManager;
import com.intellij.psi.search.FileTypeIndex;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.util.PsiTreeUtil;
import org.ax1.lisp.psi.LispFile;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import static org.ax1.lisp.psi.impl.LispSymbolMixin.SymbolSyntaxType.FUNCTION_DEFINITION;
import static org.ax1.lisp.psi.impl.LispSymbolMixin.SymbolSyntaxType.VARIABLE_USAGE;

public class LispUtil {

  /**
   * Search the entire project for function definitions.
   * @return
   */
  public static List<LispSymbol> findFunctionDefinitions(Project project, String name) {
    List<LispSymbol> result = new ArrayList<>();
    Collection<VirtualFile> virtualFiles =
        FileTypeIndex.getFiles(LispFileType.INSTANCE, GlobalSearchScope.allScope(project));
    for (VirtualFile virtualFile : virtualFiles) {
      LispFile lispFile = (LispFile) PsiManager.getInstance(project).findFile(virtualFile);
      if (lispFile != null) {
        for (LispSymbol symbol : PsiTreeUtil.findChildrenOfType(lispFile, LispSymbol.class)) {
          if (symbol.getSyntaxType() == FUNCTION_DEFINITION && symbol.getText().equals(name)) {
            result.add(symbol);
          }
        }
      }
    }
    return result;
  }

  /**
   * Search the entire project for function usages.
   */
  public static List<LispSymbol> findFunctionUsages(Project project, String name) {
    List<LispSymbol> result = new ArrayList<>();
    Collection<VirtualFile> virtualFiles =
        FileTypeIndex.getFiles(LispFileType.INSTANCE, GlobalSearchScope.allScope(project));
    for (VirtualFile virtualFile : virtualFiles) {
      LispFile lispFile = (LispFile) PsiManager.getInstance(project).findFile(virtualFile);
      if (lispFile != null) {
        for (LispSymbol symbol : PsiTreeUtil.findChildrenOfType(lispFile, LispSymbol.class)) {
          if (symbol.getText().equals(name) && symbol.isFunctionCall()) {
            result.add(symbol);
          }
        }
      }
    }
    return result;
  }

  /**
   * Search the tree upward for a variable definition.
   */
  public static PsiElement findVariableDefinition(LispSymbol symbol, String name) {
    for (LispList parent = getParentList(symbol); parent != null ; parent = getParentList(parent)) {
      if (parent.isFormDefun()) {
        for (LispSexp sexp : parent.getDefunLambdaList()) {
          LispSymbol parameter = sexp.getSymbol();
          if (parameter != null && parameter.getText().equals(name)) return parameter;
        }
        return null;
      } else if (parent.isCreatingBindings()) {
        LispSymbol bindingSymbol = parent.getBindingSymbol(name);
        if (bindingSymbol != null) return bindingSymbol;
      }
    }
    return null;
  }

  private static LispList getParentList(PsiElement node) {
    for (PsiElement parent = node.getParent(); !(parent instanceof LispFile) ; parent = parent.getParent()) {
      if (parent instanceof LispList) return (LispList) parent;
    }
    return null;
  }

  public static Collection<LispSymbol> findVariableUsages(LispSymbol variableDefinition) {
    String variableName = variableDefinition.getText();
    return variableDefinition.getVariableContainer().getFormsInScope(variableDefinition).stream()
        .flatMap(sexp -> findVariableUsages(sexp, variableName).stream())
        .collect(Collectors.toList());
  }

  private static List<LispSymbol> findVariableUsages(LispSexp sexp, String name) {
    List<LispSymbol> result = new ArrayList<>();
    LispSymbol symbol = sexp.getSymbol();
    if (symbol != null && symbol.getSyntaxType() == VARIABLE_USAGE && symbol.getText().equals(name)) {
      result.add(symbol);
    }
    LispList list = sexp.getList();
    if (list != null) {
      if (list.isFormLet() && list.getBindingSymbol(name) != null) {
        // TODO: be more accurate as to where to stop when a LET form is creating a new lexical binding.
      } else {
        for (LispSexp subsexp : list.getSexpList()) {
          result.addAll(findVariableUsages(subsexp, name));
        }
      }
    }
    return result;
  }

}
