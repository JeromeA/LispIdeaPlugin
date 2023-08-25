package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.*;
import org.ax1.lisp.analysis.symbol.LexicalSymbol;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbolName;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import static org.ax1.lisp.analysis.BaseLispElement.Type.*;
import static org.ax1.lisp.analysis.symbol.LexicalSymbol.newLexicalVariable;

public class LambdaAnalyzer {

  private static final Set<String> KEYWORDS = Set.of("&BODY", "&KEY", "&OPTIONAL", "&REST");

  /**
   * Analyze a lambda function, starting from the lambda list at the specified index in the form.
   */
  public static void analyzeLambda(String formName, LispList form, int index) {
    List<LispSexp> list = form.getSexpList();
    LispList lambdaList = list.get(index).getList();
    if (lambdaList == null) {
      list.get(index).setErrorMessage("Lambda list expected");
      return;
    }
    lambdaList.setType(CODE);
    List<LexicalSymbol> variables = getVariables(lambdaList);

    index++;
    if (index >= list.size()) return;
    // Skip documentation.
    if (list.get(index).getString() != null && index + 1 < list.size()) {
      list.get(index).setType(CODE);
      index++;
    }
    // Skip declaration.
    if (isDeclaration(list.get(index))) index++;

    SyntaxAnalyzer.INSTANCE.analyzeFormsWithVariables(list, index, variables);
  }

  private static boolean isDeclaration(LispSexp sexp) {
    LispList list = sexp.getList();
    if (list == null) return false;
    List<LispSexp> sexpList = list.getSexpList();
    if (sexpList.isEmpty()) return false;
    LispSexp firstSexp = sexpList.get(0);
    LispSymbolName firstSymbolName = firstSexp.getSymbolName();
    if (firstSymbolName == null) return false;
    return firstSymbolName.getLispName().equals("DECLARE");
  }

  @NotNull
  private static List<LexicalSymbol> getVariables(LispList lambdaList) {
    List<LexicalSymbol> result = new ArrayList<>();
    for (LispSexp parameterSpecifier : lambdaList.getSexpList()) {
      if (parameterSpecifier.isSymbol()) {
        LispSymbolName symbolName = parameterSpecifier.getSymbolName();
        if (KEYWORDS.contains(symbolName.getLispName())) {
          symbolName.setType(KEYWORD);
        } else {
          result.add(newLexicalVariable(symbolName));
        }
        continue;
      }
      LispList list = parameterSpecifier.getList();
      if (list != null && !list.getSexpList().isEmpty()) {
        List<LispSexp> varInit = list.getSexpList();
        LispSexp varName = varInit.get(0);
        if (varName.getSymbol() == null) {
          varName.setErrorMessage("Variable name expected");
          continue;
        }
        result.add(newLexicalVariable(varName.getSymbolName()));
        if (varInit.size() > 1) {
          SyntaxAnalyzer.INSTANCE.analyzeForm(varInit.get(1));
        }
        continue;
      }
      parameterSpecifier.setErrorMessage("Parameter specifier expected");
    }
    return result;
  }
}
