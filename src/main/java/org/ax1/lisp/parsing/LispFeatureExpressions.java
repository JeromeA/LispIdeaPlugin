package org.ax1.lisp.parsing;

import org.ax1.lisp.psi.*;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.stream.Collectors;

public class LispFeatureExpressions {

  public static List<LispSexp> filterOptionalSexpList(@NotNull List<LispOptionalSexp> optionalSexpList) {
    return optionalSexpList.stream()
        .filter(LispFeatureExpressions::isValidSexp)
        .map(LispOptionalSexp::getSexp)
        .collect(Collectors.toUnmodifiableList());
  }

  private static boolean isValidSexp(LispOptionalSexp optionalSexp) {
    LispFeatureExp featureExp = optionalSexp.getFeatureExp();
    return featureExp == null || LispFeatureExpressions.eval(featureExp);
  }

  public static boolean eval(LispFeatureExp featureExp) {
    boolean positive = isPositive(featureExp);
    boolean featureValue = featureExp.getSimpleFeatureExp() != null ?
        eval(featureExp.getSimpleFeatureExp()) : eval(featureExp.getCompoundFeatureExp());
    System.err.println("Sign: " + positive + "Expr: " + featureExp.getText() +
        " -> " + featureValue + " = " + (positive == featureValue));
    return positive == featureValue;
  }

  private static boolean eval(LispSimpleFeatureExp simpleFeatureExp) {
    return eval(simpleFeatureExp.getSymbolName());
  }

  private static boolean eval(@NotNull LispSymbolName symbolName) {
    return symbolName.getValue().equals("SBCL");
  }

  private static boolean eval(LispCompoundFeatureExp compoundFeatureExp) {
    List<LispSymbolName> symbolNameList = compoundFeatureExp.getSymbolNameList();
    if (symbolNameList.isEmpty()) return false;
    switch (symbolNameList.get(0).getValue()) {
      case "NOT":
        if (symbolNameList.size() != 2) return false;
        return !eval(symbolNameList.get(1));
      case "OR":
        return symbolNameList.stream().skip(1).anyMatch(LispFeatureExpressions::eval);
      case "AND":
        return symbolNameList.stream().skip(1).allMatch(LispFeatureExpressions::eval);
    }
    return false;
  }

  private static boolean isPositive(LispFeatureExp featureExp) {
    return featureExp.getText().startsWith("#+");
  }
}
