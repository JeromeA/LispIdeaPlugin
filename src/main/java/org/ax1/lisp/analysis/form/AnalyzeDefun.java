package org.ax1.lisp.analysis.form;

import com.intellij.lang.documentation.DocumentationMarkup;
import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispString;

import java.util.List;

import static com.intellij.lang.documentation.DocumentationMarkup.GRAYED_ELEMENT;
import static com.intellij.openapi.editor.DefaultLanguageHighlighterColors.FUNCTION_DECLARATION;
import static org.ax1.lisp.analysis.form.LambdaAnalyzer.analyzeLambda;

public class AnalyzeDefun implements FormAnalyzer {

  private final Type type;

  public AnalyzeDefun(Type type) {
    this.type = type;
  }

  @Override
  public void analyze(AnalysisContext context, LispList form) {
    context.highlighter.highlightKeyword(form);
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 3) {
      context.highlighter.highlightError(form, type.name() + " needs at least 2 arguments");
      return;
    }
    LispSexp functionName = list.get(1);
    if (functionName.isSymbol()) {
      context.highlighter.highlightDeclaration(functionName.getSymbolName());
      StringBuilder sb = new StringBuilder();
      sb.append(DocumentationMarkup.DEFINITION_START);
      sb.append(functionName.getText());
      LispList lambda = list.get(2).getList();
      if (lambda != null) {
        for (LispSexp sexp : lambda.getSexpList()) {
          sb.append(" ");
          sb.append(sexp.getText());
        }
      }
      sb.append(DocumentationMarkup.DEFINITION_END);
      sb.append(DocumentationMarkup.CONTENT_START);
      String docString = getDocString(list);
      if (docString == null) {
        sb.append(GRAYED_ELEMENT.addText("No doc string."));
      } else {
        sb.append(docString);
      }
      sb.append(DocumentationMarkup.CONTENT_END);
      String description = sb.toString();
      context.addFunctionDefinition(functionName.getSymbol(), description);
    } else {
      // TODO: check DEFUN SETF case.
    }
    analyzeLambda("DEFUN", context, form, 2);
  }

  private String getDocString(List<LispSexp> list) {
    if (list.size() < 4) return null;
    LispString string3 = list.get(3).getString();
    if (string3 == null) return null;
    return string3.getStringContent().getValue();
  }

  public enum Type {
    DEFUN,
    DEFMACRO
  }
}
