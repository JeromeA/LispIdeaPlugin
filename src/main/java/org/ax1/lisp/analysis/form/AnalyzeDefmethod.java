package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalyzerContext;
import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.analysis.symbol.LexicalSymbol;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbolName;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.ax1.lisp.analysis.BaseLispElement.Type.*;
import static org.ax1.lisp.analysis.symbol.LexicalSymbol.newLexicalVariable;

public class AnalyzeDefmethod implements FormAnalyzer {

  private static final Set<String> KEYWORDS =
      Stream.of("&KEY", "&OPTIONAL", "&REST")
          .collect(Collectors.toSet());

  @Override
  public void analyze(AnalyzerContext context, LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 3) {
      form.setErrorMessage("DEFMETHOD needs at least 2 arguments.");
      return;
    }
    LispSexp functionName = list.get(1);
    if (functionName.getSymbol() == null) {
      functionName.setErrorMessage("Function name expected");
      return;
    }
    functionName.getSymbolName().setType(FUNCTION_DEFINITION, context.packageContext);

    int arg = 2;
    // Skip method qualifiers.
    while (arg < list.size() && list.get(arg).getList() == null) arg++;

    if (arg == list.size()) {
      form.setErrorMessage("Missing lambda list");
      return;
    }
    LispList lambdaList = list.get(arg).getList();
    if (lambdaList == null) {
      list.get(arg).setErrorMessage("Lambda list expected");
      return;
    }

    // TODO: add CALL-NEXT-METHOD as a lexical function binding.
    SyntaxAnalyzer.INSTANCE.analyzeFormsWithVariables(context, list, arg + 1, getVariables(context, lambdaList));
  }

  @NotNull
  private static List<LexicalSymbol> getVariables(AnalyzerContext context, LispList lambdaList) {
    List<LexicalSymbol> result = new ArrayList<>();
    for (LispSexp lispSexp : lambdaList.getSexpList()) {
      LispSymbolName variable = getVariable(context, lispSexp);
      if (variable != null) result.add(newLexicalVariable(variable));
    }
    return result;
  }

  private static LispSymbolName getVariable(AnalyzerContext context, LispSexp sexp) {
    LispList list = sexp.getList();
    if (list != null) {
      List<LispSexp> specialized = list.getSexpList();
      if (specialized.size() != 2) {
        list.setErrorMessage("(var specializer) pair expected");
        return null;
      }
      if (specialized.get(0).getSymbol() == null) {
        specialized.get(0).setErrorMessage("var name expected");
        return null;
      }
      if (specialized.get(1).getSymbol() == null) {
        specialized.get(1).setErrorMessage("specializer expected");
        return null;
      }
      // TODO: check for EQL specializer.
      specialized.get(1).getSymbolName().setType(CLASS_USAGE, context.packageContext);
      sexp = specialized.get(0);
    }
    LispSymbolName symbolName = sexp.getSymbolName();
    if (symbolName == null) return null;
    if (KEYWORDS.contains(symbolName.getLispName())) {
      symbolName.setType(KEYWORD);
      return null;
    }
    return symbolName;
  }
}
