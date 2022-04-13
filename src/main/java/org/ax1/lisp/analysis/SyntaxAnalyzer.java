package org.ax1.lisp.analysis;

import com.intellij.lang.ASTNode;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.psi.PsiElement;
import org.ax1.lisp.psi.LispFile;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;
import org.jetbrains.annotations.NotNull;

import java.util.*;
import java.util.stream.Collectors;

import static com.intellij.openapi.editor.colors.CodeInsightColors.NOT_USED_ELEMENT_ATTRIBUTES;
import static com.intellij.openapi.editor.colors.CodeInsightColors.WRONG_REFERENCES_ATTRIBUTES;
import static org.ax1.lisp.analysis.SymbolDescriptor.SymbolType.FUNCTION;
import static org.ax1.lisp.analysis.SymbolDescriptor.SymbolType.VARIABLE;
import static org.ax1.lisp.parsing.LispSyntaxHighlighter.FUNCTION_DECLARATION;
import static org.ax1.lisp.parsing.LispSyntaxHighlighter.KEYWORD;
import static org.ax1.lisp.psi.LispTypes.STRING;

public class SyntaxAnalyzer {
  private final LispFile lispFile;
  private final AnnotationHolder holder;
  private String packageName = "cl-user";
  private final SymbolStack functions = new SymbolStack(FUNCTION);
  private final SymbolStack variables = new SymbolStack(VARIABLE);
  private final PackageManager packages;

  public SyntaxAnalyzer(LispFile lispFile, @NotNull AnnotationHolder holder) {
    this.lispFile = lispFile;
    this.holder = holder;
    this.packages = PackageManager.getInstance(lispFile.getProject());
  }

  public void analyze() {
    analyzeForms(lispFile.getSexpList(), 0);
    annotateSymbols(functions);
    annotateSymbols(variables);
  }

  private void annotateSymbols(SymbolStack symbols) {
    if (!symbols.getLexical().empty()) throw new RuntimeException("Unbalanced lexical stack.");
    for (SymbolDescriptor symbolDescriptor : symbols.getRetired()) {
      checkNoUsages(symbolDescriptor);
    }
    symbols.getSpecial().values().forEach(symbolDescriptor -> {
      checkNoUsages(symbolDescriptor);
      checkNoDefinition(symbolDescriptor);
    });
  }

  private void checkNoDefinition(SymbolDescriptor symbolDescriptor) {
    if (symbolDescriptor.getDefinition() == null
        && !getPackage().isSymbol(symbolDescriptor.getSymbolType(), symbolDescriptor.getName())) {
      String message = symbolDescriptor.getSymbolType() == FUNCTION ? "Function '%s' does not exist" : "Variable '%s' is not defined";
      symbolDescriptor.getUsages().forEach(usage ->
          highlightUnknown(usage, String.format(message, symbolDescriptor.getName())));
    }
  }

  private void checkNoUsages(SymbolDescriptor symbolDescriptor) {
    if (symbolDescriptor.getUsages().isEmpty()) {
      String message = symbolDescriptor.getSymbolType() == FUNCTION ? "Function '%s' is never called" : "Variable '%s' is never used";
      holder.newAnnotation(HighlightSeverity.WARNING,
              String.format(message, symbolDescriptor.getName()))
          .textAttributes(NOT_USED_ELEMENT_ATTRIBUTES)
          .range(symbolDescriptor.getDefinition())
          .create();
    }
  }

  private Package getPackage() {
    return packages.get(packageName);
  }

  private void analyzeForms(Collection<LispSexp> forms, int skip) {
    forms.stream().skip(skip).forEach(this::analyzeForm);
  }

  private void analyzeForm(LispSexp form) {
    LispSymbol symbol = form.getSymbol();
    if (symbol != null) {
      variables.registerUsage(symbol);
    }
    LispList list = form.getList();
    if (list != null) {
      analyzeCompoundForm(list);
    }
  }

  private void analyzeCompoundForm(LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.isEmpty()) return;
    LispSexp sexp0 = list.get(0);
    LispSymbol symbol0 = sexp0.getSymbol();
    if (symbol0 != null) {
      String symbolName = symbol0.getText();
      switch (symbolName) {
        case "in-package":
          analyzeInPackage(form);
          break;
        case "defun":
          analyzeDefun(form);
          break;
        case "let":
          analyzeLet(form);
          break;
        case "let*":
          analyzeLetStar(form);
          break;
        case "cond":
          analyzeCond(form);
          break;
        case "destructuring-bind":
          analyzeDestructuringBind(form);
          break;
        default:
          analyzeFunctionCall(symbol0, form);
      }
    } else {
      // TODO: handle lambda expression case.
    }
  }

  private void analyzeInPackage(LispList form) {
    highlightKeyword(form);
    if (form.getSexpList().size() != 2) {
      highlightError(form, "IN-PACKAGE needs exactly 1 argument");
      return;
    }
    LispSexp arg = form.getSexpList().get(1);
    String stringDesignator = decodeStringDesignator(arg);
    if (stringDesignator == null) {
      highlightError(arg, "Expected name designator");
      return;
    }
    if (packages.get(stringDesignator) == null) {
      highlightUnknown(arg, String.format("Unknown package '%s'", stringDesignator));
      return;
    }
    packageName = stringDesignator;
  }

  private String decodeStringDesignator(LispSexp nameDesignator) {
    if (nameDesignator.getList() != null) return null;
    LispSymbol symbol = nameDesignator.getSymbol();
    if (symbol != null) {
      String text = symbol.getText();
      int colonIndex = text.indexOf(':');
      if (colonIndex >= 0) {
        return text.substring(colonIndex + 1);
      }
      return text;
    }
    ASTNode token = nameDesignator.getFirstChild().getNode();
    if (token.getElementType() == STRING) {
      String text = token.getText();
      return text.substring(1, text.length() - 1);
    }
    return null;
  }

  private void analyzeDefun(LispList form) {
    highlightKeyword(form);
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 3) {
      highlightError(form, "DEFUN needs at least 2 arguments.");
      return;
    }
    LispSexp sexp1 = list.get(1);
    LispSymbol symbol1 = sexp1.getSymbol();
    if (symbol1 != null) {
      functions.registerSpecialDefinition(form, symbol1);
      holder.newSilentAnnotation(HighlightSeverity.INFORMATION)
          .range(symbol1)
          .textAttributes(FUNCTION_DECLARATION)
          .create();
    } else {
      // TODO: check DEFUN SETF case.
    }
    LispList lambdaList = list.get(2).getList();
    if (lambdaList == null) {
      highlightError(list.get(2), "Lambda list expected");
      return;
    }
    variables.registerLexicalDefinitions(form, lambdaList.getSexpList().stream()
        .map(LispSexp::getSymbol)
        .filter(Objects::nonNull)
        .collect(Collectors.toList()));
    analyzeForms(list, 3);
    variables.dropLexicalDefinitions();
  }

  private void analyzeDestructuringBind(LispList form) {
    highlightKeyword(form);
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 3) {
      highlightError(form, "DESTRUCTURING-BIND needs at least 2 arguments.");
      return;
    }
    LispSexp sexp1 = list.get(1);
    LispList list1 = sexp1.getList();
    if (list1 == null) {
      highlightError(sexp1, "Destructuring lambda list expected");
      return;
    }
    variables.registerLexicalDefinitions(form, getDestructuringBindVariableSymbols(list1.getSexpList()));
    analyzeForms(list, 2);
    variables.dropLexicalDefinitions();
  }

  private List<LispSymbol> getDestructuringBindVariableSymbols(@NotNull List<LispSexp> lambdaList) {
    List<LispSymbol> result = new ArrayList<>();
    for (LispSexp sexp : lambdaList) {
      LispSymbol symbol = sexp.getSymbol();
      LispList list = sexp.getList();
      if (symbol != null) {
        result.add(symbol);
      } else if (list != null) {
        result.addAll(getDestructuringBindVariableSymbols(list.getSexpList()));
      } else {
        highlightError(sexp, "Destructuring lambda list expected");
      }
    }
    return result;
  }

  private void analyzeLetStar(LispList form) {
    highlightKeyword(form);
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      highlightError(form, "LET* needs at least 1 argument");
      return;
    }
    LispList list1 = list.get(1).getList();
    if (list1 == null) {
      highlightError(list.get(1), "Variable binding list expected");
      return;
    }
    analyzeLetStarVarList(form, list1.getSexpList(), 0);
  }

  private void analyzeLetStarVarList(LispList form, @NotNull List<LispSexp> varList, int startAt) {
    if (startAt >= varList.size()) {
      analyzeForms(form.getSexpList(), 2);
    } else {
      LispSexp sexp = varList.get(startAt);
      LispSymbol symbol = sexp.getSymbol();
      LispList varWithInit = sexp.getList();
      if (symbol != null) {
        analyzeLetStarVarList(form, varList, startAt + 1);
      } else if (varWithInit != null) {
        List<LispSexp> varWithInitList = varWithInit.getSexpList();
        if (varWithInitList.size() != 2) {
          highlightError(varWithInit, "Variable binding expected");
          return;
        }
        LispSymbol variable = varWithInitList.get(0).getSymbol();
        LispSexp init = varWithInitList.get(1);
        if (variable == null) {
          highlightError(varWithInitList.get(0), "Expected variable name");
          return;
        }
        analyzeForm(init);
        variables.registerLexicalDefinitions(form, List.of(variable));
        analyzeLetStarVarList(form, varList, startAt + 1);
        variables.dropLexicalDefinitions();
      } else {
        highlightError(sexp, "Variable binding expected");
      }
    }
  }

  private void analyzeLet(LispList form) {
    highlightKeyword(form);
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      highlightError(form, "LET needs at least 1 argument");
      return;
    }
    LispList list1 = list.get(1).getList();
    if (list1 == null) {
      highlightError(list.get(1), "Variable binding list expected");
      return;
    }
    List<LispSexp> varList = list1.getSexpList();
    analyzeForms(getInitForms(varList), 0);
    variables.registerLexicalDefinitions(form, getLetVariableSymbols(varList));
    analyzeForms(list, 2);
    variables.dropLexicalDefinitions();
  }

  private Collection<LispSexp> getInitForms(List<LispSexp> varList) {
    List<LispSexp> result = new ArrayList<>();
    for (LispSexp sexp : varList) {
      LispList list = sexp.getList();
      if (list != null) {
        List<LispSexp> sexpList = list.getSexpList();
        if (sexpList.size() == 2) {
          result.add(sexpList.get(1));
        }
      }
    }
    return result;
  }

  private List<LispSymbol> getLetVariableSymbols(@NotNull List<LispSexp> varList) {
    List<LispSymbol> result = new ArrayList<>();
    for (LispSexp sexp : varList) {
      LispSymbol symbol = sexp.getSymbol();
      LispList list = sexp.getList();
      if (symbol != null) {
        result.add(symbol);
      } else if (list != null) {
        List<LispSexp> sexpList = list.getSexpList();
        if (!isVarInitValid(sexpList)) {
          highlightError(list, "Expected var init form");
          continue;
        }
        result.add(sexpList.get(0).getSymbol());
      } else {
        highlightError(sexp, "Expected var binding");
      }
    }
    return result;
  }

  private boolean isVarInitValid(List<LispSexp> sexpList) {
    if (sexpList.size() != 2) return false;
    if (sexpList.get(0).getSymbol() == null) return false;
    if (sexpList.get(1).getList() == null) return false;
    return true;
  }

  private void analyzeCond(LispList form) {
    highlightKeyword(form);
    List<LispSexp> sexpList = form.getSexpList();
    for (LispSexp sexp : sexpList.stream().skip(1).collect(Collectors.toList())) {
      LispList pair = sexp.getList();
      if (pair == null || pair.getSexpList().size() != 2) {
        highlightError(sexp, "(test-form form) pair expected");
        continue;
      }
      analyzeForm(pair.getSexpList().get(0));
      analyzeForm(pair.getSexpList().get(1));
    }
  }

  private void analyzeFunctionCall(LispSymbol symbol0, LispList form) {
    functions.registerUsage(symbol0);
    analyzeForms(form.getSexpList(), 1);
  }

  private void highlightError(PsiElement psiElement, String message) {
    holder.newAnnotation(HighlightSeverity.ERROR, message)
        .range(psiElement)
        .create();
  }

  private void highlightKeyword(LispList form) {
    holder.newSilentAnnotation(HighlightSeverity.INFORMATION)
        .range(form.getSexpList().get(0))
        .textAttributes(KEYWORD)
        .create();
  }

  private void highlightUnknown(PsiElement psiElement, String message) {
    holder.newAnnotation(HighlightSeverity.ERROR, message)
        .textAttributes(WRONG_REFERENCES_ATTRIBUTES)
        .range(psiElement)
        .create();
  }

}
