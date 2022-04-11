package org.ax1.lisp;

import com.intellij.lang.documentation.AbstractDocumentationProvider;
import com.intellij.lang.documentation.DocumentationMarkup;
import com.intellij.psi.PsiElement;
import com.intellij.psi.impl.source.tree.LeafPsiElement;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

import static com.intellij.lang.documentation.DocumentationMarkup.*;
import static org.ax1.lisp.psi.LispTypes.STRING;

public class LispDocumentationProvider extends AbstractDocumentationProvider {

  @Override
  public @Nullable @Nls String generateDoc(PsiElement element, @Nullable PsiElement originalElement) {
    if (!(element instanceof LispSymbol)) return null;
    LispSymbol symbol = (LispSymbol) element;
    if (symbol.isFunctionDefinition()) {
      return getFunctionDocumentation(symbol);
    }
    if (symbol.isVariableDefinition()) {
      return getVariableDocumentation(symbol);
    }
    return null;
  }

  private String getVariableDocumentation(LispSymbol symbol) {
    StringBuilder sb = new StringBuilder();
    sb.append(DEFINITION_ELEMENT.addText(symbol.getText()));
    sb.append(SECTIONS_START);
    if (symbol.isLetVariableName()) {
      sb.append(SECTION_HEADER_CELL.addText("Binding by:"));
      sb.append(SECTION_CONTENT_CELL.addText("LET"));
      sb.append("</tr>");
      sb.append(SECTION_HEADER_CELL.addText("Initial value:"));
      sb.append(SECTION_CONTENT_CELL.addText("--"));
    }
    if (symbol.isParameterName()) {
      LispList defun = symbol.getDefunFromParameter();
      String name = getFunctionName(defun);
      if (name != null) {
        sb.append(SECTION_HEADER_CELL.addText("Binding by:"));
        sb.append(SECTION_CONTENT_CELL.addText("parameter of function " + name));
      }
    }
    if (symbol.isDestructuringBindVariableName()) {
      sb.append(SECTION_HEADER_CELL.addText("Binding by:"));
      sb.append(SECTION_CONTENT_CELL.addText("DESTRUCTURING-BIND"));
    }
    sb.append(SECTIONS_END);
    return sb.toString();
  }

  private String getFunctionName(LispList defun) {
    List<LispSexp> sexpList = defun.getSexpList();
    if (sexpList.size() < 2) return null;
    LispSymbol symbol = sexpList.get(1).getSymbol();
    if (symbol == null) return null;
    return symbol.getText();
  }

  @NotNull
  private String getFunctionDocumentation(LispSymbol symbol) {
    StringBuilder sb = new StringBuilder();
    sb.append(DocumentationMarkup.DEFINITION_START);
    sb.append("(");
    sb.append(symbol.getText());
    LispList defun = (LispList) symbol.getParent().getParent();
    LispList lambda = getFunctionLambda(defun);
    if (lambda != null) {
      for (LispSexp sexp : lambda.getSexpList()) {
        sb.append(" ");
        sb.append(sexp.getText());
      }
    }
    sb.append(")");
    sb.append(DocumentationMarkup.DEFINITION_END);
    sb.append(DocumentationMarkup.CONTENT_START);
    String docString = getFunctionDocString(defun);
    if (docString != null) {
      sb.append(docString, 1, docString.length()-2);
    } else {
      sb.append(GRAYED_ELEMENT.addText("No doc string."));
    }
    sb.append(DocumentationMarkup.CONTENT_END);
    return sb.toString();
  }

  private String getFunctionDocString(LispList defun) {
    List<LispSexp> sexpList = defun.getSexpList();
    if (sexpList.size() <= 3) return null;
    LispSexp docString = sexpList.get(3);
    PsiElement firstChild = docString.getFirstChild();
    if (!(firstChild instanceof LeafPsiElement)) return null;
    LeafPsiElement leaf = (LeafPsiElement) firstChild;
    if (leaf.getElementType() != STRING) return null;
    return leaf.getText();
  }

  private LispList getFunctionLambda(LispList defun) {
    List<LispSexp> sexpList = defun.getSexpList();
    if (sexpList.size() < 3) return null;
    LispList lambda = sexpList.get(2).getList();
    return lambda;
  }
}
