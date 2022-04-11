package org.ax1.lisp;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.psi.PsiElement;
import org.ax1.lisp.psi.LispFile;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;
import org.ax1.lisp.psi.impl.LispSymbolMixin;
import org.jetbrains.annotations.NotNull;

import java.util.*;
import java.util.stream.Collectors;

import static org.ax1.lisp.psi.impl.LispSymbolMixin.SymbolType.*;

public class SyntaxAnalyzer {
  private final LispFile lispFile;
  private final AnnotationHolder holder;
  private String packageName = "CL-USER";
  private final SymbolStack functions = new SymbolStack(FUNCTION_DEFINITION, FUNCTION_USAGE);
  private final SymbolStack variables = new SymbolStack(VARIABLE_DEFINITION, VARIABLE_USAGE);

  public SyntaxAnalyzer(LispFile lispFile, @NotNull AnnotationHolder holder) {
    this.lispFile = lispFile;
    this.holder = holder;
  }

  public void analyze() {
    // A SymbolUsage contains:
    // - one definition (LispSymbol)
    // - multiple usages (LispSymbol)
    //
    // During analysis, there is a single Special map and a stack of Lexical maps (name to SymbolEntry).
    // Each symbol definition is added to the stack or to the Special map.
    // Each symbol usage if looked up in the Lexical map stack and resolved there, or added in the Special map.
    // Once the analysis is done, all the name->Symbol maps are converted into LispSymbol->SymbolEntry and stored in
    // LispFile.
    analyzeForms(lispFile.getSexpList(), 0);
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
        case "defun":
          analyzeDefun(form);
          break;
        case "let":
          analyzeLet(form);
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

  private void analyzeDefun(LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 3) {
      error(form, "DEFUN needs at least 2 arguments.");
      return;
    }
    LispSexp sexp1 = list.get(1);
    LispSymbol symbol1 = sexp1.getSymbol();
    if (symbol1 != null) {
      functions.registerSpecialDefinition(form, symbol1);
      LispList lambdaList = list.get(2).getList();
      if (lambdaList == null) {
        error(list.get(2), "Lambda list expected");
        return;
      }
      variables.registerLexicalDefinitions(form, lambdaList.getSexpList().stream()
                .map(LispSexp::getSymbol)
                .filter(Objects::nonNull)
                .collect(Collectors.toList()));
      analyzeForms(list, 3);
      variables.dropLexicalDefinitions();
    } else {
      // TODO: check SETF case.
    }
  }

  private void analyzeDestructuringBind(LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 3) {
      error(form, "DESTRUCTURING-BIND needs at least 2 arguments.");
      return;
    }
    LispSexp sexp1 = list.get(1);
    LispList list1 = sexp1.getList();
    if (list1 == null) {
      error(sexp1, "Destructuring lambda list expected");
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
      if (symbol != null) {
        result.add(symbol);
      }
      LispList list = sexp.getList();
      if (list != null) {
        result.addAll(getDestructuringBindVariableSymbols(list.getSexpList()));
      }
    }
    return result;
  }

  private void analyzeLet(LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      error(form, "LET needs at least 1 argument");
      return;
    }
    LispList list1 = list.get(1).getList();
    if (list1 == null) {
      error(list.get(1), "Variable binding list expected");
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
      if (symbol != null) {
        result.add(symbol);
      }
      LispList list = sexp.getList();
      if (list != null) {
        List<LispSexp> sexpList = list.getSexpList();
        if (!isVarInitValid(sexpList)) {
          error(list, "Invalid var init form");
          continue;
        }
        result.add(sexpList.get(0).getSymbol());
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
    List<LispSexp> sexpList = form.getSexpList();
    for (LispSexp sexp : sexpList.stream().skip(1).collect(Collectors.toList())) {
      LispList pair = sexp.getList();
      if (pair == null || pair.getSexpList().size() != 2) {
        error(sexp, "(test-form form) pair expected");
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

  private void error(PsiElement psiElement, String message) {
    holder.newAnnotation(HighlightSeverity.ERROR, message)
        .range(psiElement)
        .create();
  }

  private static class SymbolStack {

    private final Map<String, SymbolCache> special = new HashMap<>();
    private final Stack<Map<String, SymbolCache>> lexical = new Stack<>();
    private final List<SymbolCache> retired = new ArrayList<>();
    private final LispSymbolMixin.SymbolType definition;
    private final LispSymbolMixin.SymbolType usage;

    public SymbolStack(LispSymbolMixin.SymbolType definition, LispSymbolMixin.SymbolType usage) {
      this.definition = definition;
      this.usage = usage;
    }

    public void registerUsage(LispSymbol symbol) {
      String symbolName = symbol.getText();
      SymbolCache symbolCache = getSymbol(symbolName);
      symbolCache.addUsage(symbol);
      symbol.setSymbol(usage, symbolCache);
    }

    public void registerSpecialDefinition(LispList container, LispSymbol symbol) {
      String symbolName = symbol.getText();
      SymbolCache symbolCache = special.get(symbolName);
      if (symbolCache == null) {
        symbolCache = new SymbolCache();
        special.put(symbolName, symbolCache);
      }
      symbolCache.setDefinition(container, symbol);
      symbol.setSymbol(definition, symbolCache);
    }

    public void registerLexicalDefinitions(LispList container, List<LispSymbol> variableList) {
      Map<String, SymbolCache> newDictionary = new HashMap<>();
      for (LispSymbol symbol : variableList) {
        String symbolName = symbol.getText();
        SymbolCache symbolCache = new SymbolCache();
        symbolCache.setDefinition(container, symbol);
        newDictionary.put(symbolName, symbolCache);
        symbol.setSymbol(definition, symbolCache);
      }
      lexical.push(newDictionary);
    }

    public void dropLexicalDefinitions() {
      retired.addAll(lexical.pop().values());
    }

    private SymbolCache getSymbol(String symbolName) {
      for (int i = lexical.size()-1 ; i >= 0 ; i--) {
        SymbolCache symbol = lexical.get(i).get(symbolName);
        if (symbol != null) return symbol;
      }
      SymbolCache symbol = special.get(symbolName);
      if (symbol == null) {
        symbol = new SymbolCache();
        special.put(symbolName, symbol);
      }
      return symbol;
    }
  }
}
