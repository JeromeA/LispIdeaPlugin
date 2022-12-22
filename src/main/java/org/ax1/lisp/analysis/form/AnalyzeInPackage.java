package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.impl.LispStringDesignator;

import java.util.Locale;
import java.util.function.Function;

import static org.ax1.lisp.analysis.SyntaxAnalyzer.getCompletionPrefix;
import static org.ax1.lisp.analysis.SyntaxAnalyzer.isCompletion;

public class AnalyzeInPackage implements FormAnalyzer {

  @Override
  public void analyze(AnalysisContext context, LispList form) {
    context.highlighter.highlightKeyword(form);
    if (form.getSexpList().size() != 2) {
      context.highlighter.highlightError(form, "IN-PACKAGE needs exactly 1 argument");
      return;
    }
    LispSexp sexp1 = form.getSexpList().get(1);
    LispStringDesignator stringDesignator = sexp1.getStringDesignator();
    if (stringDesignator == null) {
      context.highlighter.highlightError(sexp1, "Expected name designator");
      return;
    }
    if (isCompletion(sexp1)) {
      String prefix = getCompletionPrefix(sexp1);
      if (prefix.isEmpty()) {
        addCompletions(context, p -> "#:" + p.toLowerCase());
      } else if (prefix.equals("#")) {
        addCompletions(context, p -> ":" + p.toLowerCase());
      } else if (prefix.startsWith(":") || prefix.startsWith("#:")) {
        addCompletions(context, String::toLowerCase);
      } else  if (prefix.startsWith("\"")) {
        addCompletions(context, Function.identity());
      }
    }

    context.result.addPackageUsage(stringDesignator);
    context.setCurrentPackage(stringDesignator.getValue());
  }

  private void addCompletions(AnalysisContext context, Function<String, String> stringMapper) {
    context.packageManager.packages.keySet().stream()
        .map(stringMapper)
        .forEach(p -> context.analyzer.completions.add(p));
  }
}
