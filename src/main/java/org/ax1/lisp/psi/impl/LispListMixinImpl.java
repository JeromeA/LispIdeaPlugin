package org.ax1.lisp.psi.impl;

import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import org.ax1.lisp.psi.*;
import org.ax1.lisp.psi.impl.LispSymbolMixin.SymbolSyntaxType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import static org.ax1.lisp.psi.impl.LispListMixin.ListSyntaxType.*;
import static org.ax1.lisp.psi.impl.LispSymbolMixin.SymbolSyntaxType.*;


public abstract class LispListMixinImpl extends ASTWrapperPsiElement implements LispList {

  private ListSyntaxType syntaxType = ListSyntaxType.UNKNOWN;

  public LispListMixinImpl(@NotNull ASTNode node) {
    super(node);
  }

  public boolean isCreatingBindings() {
    return isFormDefun() || isFormLet() || isFormDestructuringBind() || isFormDefun();
  }

  public boolean isFormDefun() {
    if (!Objects.equals(getFunctionCallName(), "defun")) return false;
    List<LispSexp> children = getSexpList();
    if (children.size() < 2) return false;
    LispSymbol symbol1 = children.get(1).getSymbol();
    return symbol1 != null;
  }

  public @NotNull List<LispSexp> getDefunLambdaList() {
    return getSexpList().get(2).getList().getSexpList();
  }

  public boolean isFormLet() {
    String functionCallName = getFunctionCallName();
    if (!Objects.equals(functionCallName, "let") && !Objects.equals(functionCallName, "let*")) return false;
    List<LispSexp> children = getSexpList();
    if (children.size() < 2) return false;
    LispList variableList = children.get(1).getList();
    return variableList != null;
  }

  public @NotNull List<LispSexp> getVariableList() {
    return getSexpList().get(1).getList().getSexpList();
  }

  public boolean isFormDestructuringBind() {
    if (!Objects.equals(getFunctionCallName(), "destructuring-bind")) return false;
    List<LispSexp> children = getSexpList();
    if (children.size() < 2) return false;
    LispList variableList = children.get(1).getList();
    return variableList != null;
  }

  public LispList getDestructuringBindVariableList() {
    return getSexpList().get(1).getList();
  }

  @Nullable
  private String getFunctionCallName() {
    if (getSyntaxType() != FUNCTION_CALL) return null;
    List<LispSexp> children = getSexpList();
    if (children.isEmpty()) return null;
    LispSymbol symbol0 = children.get(0).getSymbol();
    if (symbol0 == null) return null;
    String functionCallName = symbol0.getText();
    return functionCallName;
  }

  public boolean isFunctionCall() {
    LispSexp sexp = (LispSexp) getParent();
    PsiElement parent = sexp.getParent();
    if (parent instanceof LispFile) {
      return true;
    }
    LispList parentList = (LispList) parent;
    return parentList.isFunctionCall();
  }

  public ListSyntaxType getSyntaxType() {
    if (syntaxType == ListSyntaxType.UNKNOWN) {
      ((LispFile) getContainingFile()).computeSyntaxType();
    }
    return syntaxType;
  }

  public void setSyntaxType(ListSyntaxType syntaxType) {
    this.syntaxType = syntaxType;
    switch (syntaxType) {
      case DATA:
        propagateSyntaxType(SymbolSyntaxType.DATA, ListSyntaxType.DATA, 0);
        break;
      case FUNCTION_CALL:
        propagateFunctionCall();
        break;
      case VAR_INIT:
        propagateVarInitSyntaxType();
        break;
      case STRUCTURE:
        break;
      case UNKNOWN:
      default:
        throw new RuntimeException("Invalid value for setSyntaxType: " + syntaxType);
    }
  }

  @Override
  public LispSymbol getBindingSymbol(String name) {
    for (LispSexp variableSexp : getVariableList()) {
      LispSymbol simpleVariable = variableSexp.getSymbol();
      if (simpleVariable != null && simpleVariable.getText().equals(name)) return simpleVariable;
      LispList variableWithInit = variableSexp.getList();
      if (variableWithInit != null) {
        List<LispSexp> variableWithInitSexpList = variableWithInit.getSexpList();
        if (!variableWithInitSexpList.isEmpty()) {
          LispSymbol variableName = variableWithInitSexpList.get(0).getSymbol();
          if (variableName != null && variableName.getText().equals(name)) return variableName;
        }
      }
    }
    return null;
  }

  private void propagateFunctionCall() {
    LispSymbol symbol = getSexpList().get(0).getSymbol();
    if (symbol == null) {
      propagateSyntaxType(SymbolSyntaxType.DATA, ListSyntaxType.DATA, 0);
      return;
    }
    symbol.setSyntaxType(FUNCTION_USAGE);
    switch (symbol.getText()) {
      case "defun":
        propagateDefun();
        break;
      case "let":
      case "let*":
        propagateLet();
        break;
      case "cond":
        propagateCond();
        break;
      case "destructuring-bind":
        propagateDestructuringBind();
        break;
      default:
        propagateFunctionCallSyntaxType();
        break;
    }
  }

  private void propagateLet() {
    List<LispSexp> sexpList = getSexpList();
    if (sexpList.size() < 2) return;
    LispList list = sexpList.get(1).getList();
    if (list != null) {
      list.setSyntaxType(STRUCTURE);
      propagateSyntaxType(list.getSexpList(), VARIABLE_DEFINITION, VAR_INIT, 0);
    }
    propagateSyntaxType(VARIABLE_USAGE, FUNCTION_CALL, 2);
  }

  private void propagateDestructuringBind() {
    List<LispSexp> sexpList = getSexpList();
    if (sexpList.size() < 2) return;
    LispList list = sexpList.get(1).getList();
    if (list != null) propagateToAllSymbols(list, VARIABLE_DEFINITION);
    propagateSyntaxType(VARIABLE_USAGE, FUNCTION_CALL, 2);
  }

  /** Mark a whole tree of symbols as the given type */
  private void propagateToAllSymbols(LispList list, SymbolSyntaxType syntaxType) {
    list.setSyntaxType(STRUCTURE);
    List<LispSexp> sexpList1 = list.getSexpList();
    sexpList1.stream()
        .map(LispSexp::getSymbol)
        .filter(Objects::nonNull)
        .forEach(symbol -> symbol.setSyntaxType(syntaxType));
    sexpList1.stream()
        .map(LispSexp::getList)
        .filter(Objects::nonNull)
        .forEach(sublist -> propagateToAllSymbols(sublist, syntaxType));
  }

  private void propagateCond() {
    // Starting from the second element, each sexp is a clause, and each clause has 2 or more executable forms.
    List<LispSexp> allSexp = getSexpList().stream()
        .skip(1) // Skip COND symbol.
        .map(LispSexp::getList)
        .filter(Objects::nonNull)
        .peek(clause -> clause.setSyntaxType(STRUCTURE))
        .flatMap(clause -> clause.getSexpList().stream()).collect(Collectors.toList());
    propagateSyntaxType(allSexp, VARIABLE_USAGE, FUNCTION_CALL, 0);
  }

  private void propagateDefun() {
    List<LispSexp> sexpList = getSexpList();
    if (sexpList.size() < 2) return;
    LispSymbol symbol = sexpList.get(1).getSymbol();
    if (symbol != null) symbol.setSyntaxType(FUNCTION_DEFINITION);
    if (sexpList.size() < 3) return;
    LispList lambdaList = sexpList.get(2).getList();
    if (lambdaList != null) {
      lambdaList.setSyntaxType(STRUCTURE);
      lambdaList.getSexpList().stream()
          .map(LispSexp::getSymbol)
          .filter(Objects::nonNull)
          .forEach(s -> s.setSyntaxType(VARIABLE_DEFINITION));
    }
    propagateSyntaxType(VARIABLE_USAGE, FUNCTION_CALL, 3);
  }

  private void propagateFunctionCallSyntaxType() {
    LispSymbol symbol = getSexpList().get(0).getSymbol();
    if (symbol != null) symbol.setSyntaxType(FUNCTION_USAGE);
    propagateSyntaxType(VARIABLE_USAGE, FUNCTION_CALL, 1);
  }

  private void propagateVarInitSyntaxType() {
    List<LispSexp> sexpList = getSexpList();
    if (!sexpList.isEmpty()) {
      LispSymbol name = sexpList.get(0).getSymbol();
      if (name != null) name.setSyntaxType(VARIABLE_DEFINITION);
      if (sexpList.size() >= 2) {
        LispList init = sexpList.get(1).getList();
        if (init != null) init.setSyntaxType(FUNCTION_CALL);
      }
    }
  }

  private void propagateSyntaxType(SymbolSyntaxType symbolType, ListSyntaxType listType, int skip) {
    propagateSyntaxType(getSexpList(), symbolType, listType, skip);
  }

  private static void propagateSyntaxType(List<LispSexp> sexpList, SymbolSyntaxType symbolType, ListSyntaxType listType, int skip) {
    sexpList.stream()
        .skip(skip)
        .map(LispSexp::getSymbol)
        .filter(Objects::nonNull)
        .forEach(symbol -> symbol.setSyntaxType(symbolType));
    sexpList.stream()
        .skip(skip)
        .map(LispSexp::getList)
        .filter(Objects::nonNull)
        .forEach(list -> list.setSyntaxType(listType));
  }

  public List<LispSexp> getFormsInScope(LispSymbol variable) {
    List<LispSexp> sexpList = getSexpList();
    String name = sexpList.get(0).getSymbol().getText();
    switch (name) {
      case "let":
        return sexpList.stream().skip(2).collect(Collectors.toList());
      case "let*":
        // TODO: improve accuracy.
        return sexpList.stream().skip(2).collect(Collectors.toList());
      case "defun":
      case "destructuring-bind":
        return sexpList.stream().skip(3).collect(Collectors.toList());
      default:
        throw new RuntimeException("getFormsInScope request on invalid form " + name);
    }
  }
}
