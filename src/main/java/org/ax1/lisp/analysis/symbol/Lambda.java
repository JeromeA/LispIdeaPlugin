package org.ax1.lisp.analysis.symbol;

import com.intellij.openapi.project.Project;
import org.ax1.lisp.psi.*;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Lambda {

  public int requiredCount = 0;
  public int optionalCount = 0;
  public boolean hasRest = false;
  public Set<String> keys = new HashSet<>();
  public String string;

  private Lambda() {}

  public static Lambda from(Project project, String lambdaString) {
    return from(toLispList(project, lambdaString));
  }

  public static Lambda from(LispList sexp) {
    Lambda lambda = new Lambda();
    lambda.string = sexp.getText();
    List<LispSexp> list = sexp.getSexpList();
    int index = 0;
    while (index < list.size() && isVariableName(list.get(index))) {
      index++;
      lambda.requiredCount++;
    }
    if (index < list.size() && isOptionalKeyword(list.get(index))) {
      index++;
      while (index < list.size() && isOptionalName(list.get(index))) {
        index++;
        lambda.optionalCount++;
      }
    }
    if (index < list.size() && isRestKeyword(list.get(index))) {
      lambda.hasRest = true;
      index += 2;
    }
    if (index < list.size() && isKeyKeyword(list.get(index))) {
      index++;
      while (index < list.size()) {
        String keyName = getKeyName(list.get(index));
        if (keyName == null) break;
        lambda.keys.add(keyName);
        index++;
      }
    }
    return index < list.size() ? null : lambda;
  }

  private static String getKeyName(LispSexp sexp) {
    // Lone var symbol.
    LispSymbolName symbolName = sexp.getSymbolName();
    if (symbolName != null) return symbolName.getValue();

    // (var init-form)
    LispList list = sexp.getList();
    if (list == null) return null;
    List<LispSexp> sexpList = list.getSexpList();
    if (sexpList.isEmpty()) return null;
    LispSexp sexp0 = sexpList.get(0);
    symbolName = sexp0.getSymbolName();
    if (symbolName != null) return symbolName.getValue();

    // ((keyword var) init-form)
    LispList innerList = sexp0.getList();
    if (innerList == null) return null;
    List<LispSexp> innerSexpList = innerList.getSexpList();
    if (innerSexpList.isEmpty()) return null;
    symbolName = innerSexpList.get(0).getSymbolName();
    if (symbolName != null) return symbolName.getValue();
    return null;
  }

  private static boolean isVariableName(LispSexp sexp) {
    LispSymbol symbol = sexp.getSymbol();
    return symbol != null && !symbol.getSymbolName().getValue().startsWith("&");
  }

  private static boolean isOptionalKeyword(LispSexp sexp) {
    LispSymbol symbol = sexp.getSymbol();
    return symbol != null && symbol.getSymbolName().getValue().equals("&OPTIONAL");
  }

  private static boolean isKeyKeyword(LispSexp sexp) {
    LispSymbol symbol = sexp.getSymbol();
    return symbol != null && symbol.getSymbolName().getValue().equals("&KEY");
  }

  private static boolean isRestKeyword(LispSexp sexp) {
    LispSymbol symbol = sexp.getSymbol();
    if (symbol == null) return false;
    String name = symbol.getSymbolName().getValue();
    return name.equals("&REST") || name.equals("&BODY");
  }

  private static boolean isOptionalName(LispSexp sexp) {
    return isVariableName(sexp) || sexp.getList() != null;
  }

  private static LispList toLispList(Project project, String lambdaString) {
    LispOptionalSexp topOptionalSexp = (LispOptionalSexp) LispElementFactory.createFile(project, lambdaString).getFirstChild();
    return topOptionalSexp.getSexp().getList();
  }
}
