package org.ax1.lisp;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.Annotator;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.psi.PsiElement;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.Set;

import static org.ax1.lisp.parsing.LispSyntaxHighlighter.FUNCTION_DECLARATION;
import static org.ax1.lisp.parsing.LispSyntaxHighlighter.KEYWORD;

public class LispAnnotator implements Annotator {

  private static final Set<String> KEYWORDS =
      Set.of("cond", "defun", "destructuring-bind", "if", "in-package", "let", "let*", "loop", "unless", "when");

  @Override
  public void annotate(@NotNull PsiElement element, @NotNull AnnotationHolder holder) {
    annotateKnownForm(element, holder);
  }

  private void annotateKnownForm(@NotNull PsiElement element, @NotNull AnnotationHolder holder) {
    if (!(element instanceof LispList)) return;
    LispList list = (LispList) element;
    List<LispSexp> children = list.getSexpList();
    if (children.isEmpty()) return;
    LispSymbol symbol0 = children.get(0).getSymbol();
    if (symbol0 == null) return;
    String formName = symbol0.getText();
    if (KEYWORDS.contains(formName)) {
      holder.newSilentAnnotation(HighlightSeverity.INFORMATION)
          .range(symbol0)
          .textAttributes(KEYWORD)
          .create();
    }
    switch (formName) {
      case "cond":
        annotateCond(list, holder);
        break;
      case "defun":
        annotateDefun(list, holder);
        break;
      case "destructuring-bind":
        annotateDestructuringBind(list, holder);
        break;
      case "let":
      case "let*":
        annotateLet(list, holder);
        break;
    }
  }

  private void annotateDestructuringBind(LispList form, AnnotationHolder holder) {
    List<LispSexp> children = form.getSexpList();
    if (children.size() < 3) {
      holder.newAnnotation(HighlightSeverity.ERROR, "DESTRUCTURING-BIND needs at least 2 arguments.")
          .range(form)
          .create();
      return;
    }
  }

  private void annotateCond(LispList form, AnnotationHolder holder) {
    List<LispSexp> children = form.getSexpList();
    if (children.size() < 2) {
      holder.newAnnotation(HighlightSeverity.ERROR, "COND needs at least 1 argument.")
          .range(form)
          .create();
      return;
    }
    LispList varList = children.get(1).getList();
    if (varList == null) {
      holder.newAnnotation(HighlightSeverity.ERROR, "COND's argument 1 must be a clause list.")
          .range(children.get(1))
          .create();
      return;
    }
  }


  private void annotateLet(LispList form, AnnotationHolder holder) {
    List<LispSexp> children = form.getSexpList();
    if (children.size() < 2) {
      holder.newAnnotation(HighlightSeverity.ERROR, "LET needs at least 1 argument.")
          .range(form)
          .create();
      return;
    }
    LispList varList = children.get(1).getList();
    if (varList == null) {
      holder.newAnnotation(HighlightSeverity.ERROR, "LET's argument 1 must be a variable binding list.")
          .range(children.get(1))
          .create();
      return;
    }
  }

  private void annotateDefun(@NotNull LispList form, @NotNull AnnotationHolder holder) {
    List<LispSexp> children = form.getSexpList();
    if (children.size() < 2) {
      holder.newAnnotation(HighlightSeverity.ERROR, "DEFUN needs at least 2 arguments.")
          .range(form)
          .create();
      return;
    }
    LispSymbol symbol1 = children.get(1).getSymbol();
    if (symbol1 == null) {
      holder.newAnnotation(HighlightSeverity.ERROR, "DEFUN's argument 1 must be a function-name.")
          .range(children.get(1))
          .create();
      return;
    }
    holder.newSilentAnnotation(HighlightSeverity.INFORMATION)
        .range(children.get(1))
        .textAttributes(FUNCTION_DECLARATION)
        .create();
    if (children.size() < 3) {
      holder.newAnnotation(HighlightSeverity.ERROR, "DEFUN needs at least 2 arguments.")
          .range(form)
          .create();
      return;
    }
    LispList list2 = children.get(2).getList();
    if (list2 == null) {
      holder.newAnnotation(HighlightSeverity.ERROR, "DEFUN's argument 2 must be a lambda-list.")
          .range(children.get(2))
          .create();
      return;
    }
  }
}
