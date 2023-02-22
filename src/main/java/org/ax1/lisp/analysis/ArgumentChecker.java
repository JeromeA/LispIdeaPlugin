package org.ax1.lisp.analysis;

import com.intellij.psi.PsiElement;
import org.ax1.lisp.analysis.symbol.Lambda;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbolName;
import org.ax1.lisp.psi.impl.LispStringDesignator;

import java.util.List;

public class ArgumentChecker {

  private Symbol symbol;
  private Lambda lambda;
  private Highlighter highlighter;

  public ArgumentChecker(Symbol symbol, Lambda lambda, Highlighter highlighter) {
    this.symbol = symbol;
    this.lambda = lambda;
    this.highlighter = highlighter;
  }

  public void checkArguments(LispStringDesignator usage) {
    // The StringDesignator is a SymbolName, the first parent is a Symbol, the second parent is a Sexp, and the third
    // parent is the list we are looking for.
    PsiElement container = usage.getParent().getParent().getParent();
    // If the container is a #', we are not interested in checking arguments.
    if (!(container instanceof LispList)) return;
    LispList form = (LispList) container;
    List<LispSexp> sexpList = form.getSexpList();
    if (sexpList.size() - 1 < lambda.requiredCount) {
      highlighter.highlightError(form, String.format("%s requires %d arguments", symbol.getQualifiedName(), lambda.requiredCount));
      return;
    }
    if (lambda.hasRest) return;
    int optionalCount = 0;
    boolean keywordsStarted = false;
    for (int i = 1 + lambda.requiredCount; i < sexpList.size(); i++) {
      LispSexp sexp = sexpList.get(i);
      if (isKeyword(sexp)) {
        keywordsStarted = true;
        if (i + 1 == sexpList.size()) {
          highlighter.highlightError(sexp, String.format("Missing value for keyword %s", sexp.getText()));
          return;
        }
        // Skip value for this keyword
        i++;
      } else {
        // TODO: Distinguish 4 cases:
        // - optionals remaining, keywords not started: consume it
        // - optionals remaining, keywords started: invalid keyword
        // - no optionals remaining and it looks like a keyword: invalid keyword
        // - no optionals, and not keyaord: too many arguments
        if (keywordsStarted) {
          highlighter.highlightError(sexp, "Invalid keyword");
          return;
        } else {
          optionalCount++;
          if (optionalCount > lambda.optionalCount) {
            highlighter.highlightError(form, "Too many arguments");
          }
        }
      }
    }
  }

  private boolean isKeyword(LispSexp lispSexp) {
    LispSymbolName symbolName = lispSexp.getSymbolName();
    if (symbolName == null) return false;
    String name = symbolName.getValue();
    return lambda.keys.contains(name);
  }
}
