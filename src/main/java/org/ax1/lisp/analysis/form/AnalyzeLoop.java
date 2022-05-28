package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.LexicalBindingManager.LexicalScope;
import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * http://www.lispworks.com/documentation/lw51/CLHS/Body/m_loop.htm
 *
 * Accumulation statements can create lexical binding that are valid for the scope of all main clauses, so their
 * bindings are currently ignored.
 *
 * TODO: add a first pass that detects accumulation lexical bindings.
 * TODO: rewrite the main pass using a grammar as this code is very error-prone.
 */
public class AnalyzeLoop implements FormAnalyzer {

  private static final Set<String> FOR_SUBCLAUSE_KEYWORDS = Set.of("=", "across", "below", "from", "in", "on", "to");


  @Override
  public void analyze(SyntaxAnalyzer analyzer, LispList form) {
    analyzer.annotations.highlightKeyword(form.getSexpList().get(0));
    if (form.getSexpList().size() == 1) return;
    LispSexp firstSexp = form.getSexpList().get(1);
    if (firstSexp.getList() != null) {
      analyzer.analyzeForms(form.getSexpList(), 1);
    } else {
      nameClause(analyzer, form);
    }
  }

  /**
   * Parse an optional name clause (named name), on continue with variable clauses.
   */
  private void nameClause(SyntaxAnalyzer analyzer, LispList form) {
    List<LispSexp> list = form.getSexpList();
    LispSymbol symbol = list.get(1).getSymbol();
    if (symbol == null) {
      analyzer.annotations.highlightError(list.get(1), "Loop keyword expected");
      return;
    }
    if (symbol.getText().equals("named")) {
      if (list.size() <= 2) {
        analyzer.annotations.highlightError(form, "Name missing");
        return;
      }
      if (list.get(2).getSymbol() == null) {
        analyzer.annotations.highlightError(list.get(2), "Name expected");
        return;
      }
      variableClause(analyzer, form, 3);
    }
    variableClause(analyzer, form, 1);
  }

  /**
   * Parse optional variable clauses, and continue with main clauses.
   */
  private void variableClause(SyntaxAnalyzer analyzer, LispList form, int startAt) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() <= startAt) return;
    LispSymbol symbol = list.get(startAt).getSymbol();
    if (symbol == null) {
      analyzer.annotations.highlightError(list.get(startAt), "Loop keyword expected");
      return;
    }
    switch (symbol.getText()) {
      case "with":
        with(analyzer, form, startAt);
        break;
      case "initially":
      case "finally":
        initiallyFinally(analyzer, form, startAt, false);
        break;
      case "for":
      case "as":
        forAs(analyzer, form, startAt);
        break;
      default:
        mainClause(analyzer, form, startAt);
        break;
    }
  }

  private void initiallyFinally(SyntaxAnalyzer analyzer, LispList form, int startAt, boolean inMainClause) {
    List<LispSexp> list = form.getSexpList();
    analyzer.annotations.highlightKeyword(list.get(startAt));
    startAt++;
    if (list.size() <= startAt) {
      analyzer.annotations.highlightError(list.get(startAt - 1), "Form missing");
      return;
    }
    while (list.size() > startAt && list.get(startAt).getList() != null) {
      analyzer.analyzeForm(list.get(startAt));
      startAt++;
    }
    if (inMainClause) {
      mainClause(analyzer, form, startAt);
    } else {
      variableClause(analyzer, form, startAt);
    }
  }

  private void doDoing(SyntaxAnalyzer analyzer, LispList form, int startAt) {
    int consumed = doDoingRaw(analyzer, form, startAt);
    if (consumed == 0) return;
    mainClause(analyzer, form, startAt + consumed);
  }

  /**
   * Parse a do/doing statement. Such a statement can have any number of compound forms to execute.
   */
  private int doDoingRaw(SyntaxAnalyzer analyzer, LispList form, int startAt) {
    List<LispSexp> list = form.getSexpList();
    analyzer.annotations.highlightKeyword(list.get(startAt));
    int consumed = 1;
    if (list.size() <= startAt + consumed) {
      analyzer.annotations.highlightError(list.get(startAt - 1), "Form missing");
      return 0;
    }
    while (list.size() > startAt + consumed && list.get(startAt + consumed).getList() != null) {
      analyzer.analyzeForm(list.get(startAt + consumed));
      consumed++;
    }
    return consumed;
  }

  private void with(SyntaxAnalyzer analyzer, LispList form, int startAt) {
    List<LispSexp> list = form.getSexpList();
    analyzer.annotations.highlightKeyword(list.get(startAt));
    startAt++;

    if (list.size() <= startAt) {
      analyzer.annotations.highlightError(list.get(startAt - 1), "Variable missing");
      return;
    }
    LispSexp sexp1 = list.get(startAt);
    if (sexp1.getSymbol() == null) {
      analyzer.annotations.highlightError(sexp1, "Variable name expected");
      return;
    }
    startAt++;

    if (list.size() <= startAt) return;
    LispSexp sexp2 = list.get(startAt);
    if (sexp2.getSymbol() == null) {
      analyzer.annotations.highlightError(sexp2, "Loop keyword expected");
      return;
    }
    if (sexp2.getSymbol().getText().equals("=")) {
      analyzer.annotations.highlightKeyword(sexp2);
      analyzer.analyzeForm(list.get(startAt + 1));
      startAt += 2;
    }

    try (LexicalScope lexicalScope = analyzer.lexicalBindings.defineLexicalVariables(form, List.of(sexp1.getSymbol()))) {
      variableClause(analyzer, form, startAt);
    }
  }

  private void forAs(SyntaxAnalyzer analyzer, LispList form, int startAt) {
    List<LispSexp> list = form.getSexpList();
    analyzer.annotations.highlightKeyword(list.get(startAt));
    startAt++;

    if (list.size() <= startAt) {
      analyzer.annotations.highlightError(list.get(startAt - 1), "Variable name missing");
      return;
    }
    LispSexp sexp1 = list.get(startAt);
    List<LispSymbol> variables = getVariables(analyzer, sexp1);
    startAt++;

    while (true) {
      if (list.size() <= startAt) return;
      LispSexp sexp2 = list.get(startAt);
      if (sexp2.getSymbol() == null) {
        analyzer.annotations.highlightError(sexp2, "Loop keyword expected");
        return;
      }
      if (!FOR_SUBCLAUSE_KEYWORDS.contains(sexp2.getSymbol().getText())) break;
      analyzer.annotations.highlightKeyword(sexp2);
      analyzer.analyzeForm(list.get(startAt + 1));
      startAt += 2;
    }

    try (LexicalScope lexicalScope = analyzer.lexicalBindings.defineLexicalVariables(form, variables)) {
      variableClause(analyzer, form, startAt);
    }
  }

  private List<LispSymbol> getVariables(SyntaxAnalyzer analyzer, LispSexp sexp) {
    LispSymbol symbol = sexp.getSymbol();
    if (symbol != null) {
      if (symbol.getText().equals("nil")) {
        analyzer.annotations.highlightKeyword(symbol);
        return List.of();
      }
      return List.of(symbol);
    }
    LispList list = sexp.getList();
    if (list != null) {
      List<LispSexp> sexpList = list.getSexpList();
      return sexpList.stream().flatMap(s -> getVariables(analyzer, s).stream()).collect(Collectors.toList());
    }
    analyzer.annotations.highlightError(sexp, "Variable name expected");
    return List.of();
  }


  private void mainClause(SyntaxAnalyzer analyzer, LispList form, int startAt) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() <= startAt) return;
    LispSexp sexp = list.get(startAt);
    if (sexp.getSymbol() == null) {
      analyzer.annotations.highlightError(sexp, "Loop keyword expected");
      return;
    }
    switch (sexp.getSymbol().getText()) {
      case "do":
      case "doing":
        doDoing(analyzer, form, startAt);
        break;
      case "return":
        aReturn(analyzer, form, startAt);
        break;
      case "append":
      case "appending":
      case "collect":
      case "collecting":
      case "count":
      case "counting":
      case "maximize":
      case "maximizing":
      case "minimize":
      case "minimizing":
      case "nconc":
      case "nconcing":
      case "sum":
      case "summing":
        accumulation(analyzer, form, startAt);
        break;
      case "initially":
      case "finally":
        initiallyFinally(analyzer, form, startAt, true);
        break;
      case "always":
      case "never":
      case "repeat":
      case "thereis":
      case "until":
      case "while":
        termination(analyzer, form, startAt);
        break;
      case "if":
      case "when":
      case "unless":
        conditional(analyzer, form, startAt);
        break;
      default:
        analyzer.annotations.highlightError(sexp, "Loop keyword expected");
    }
  }

  private void conditional(SyntaxAnalyzer analyzer, LispList form, int startAt) {
    int consumed = conditionalRaw(analyzer, form, startAt);
    if (consumed == 0) return;
    mainClause(analyzer, form, startAt + consumed);
  }

  private int conditionalRaw(SyntaxAnalyzer analyzer, LispList form, int startAt) {
    List<LispSexp> list = form.getSexpList();
    analyzer.annotations.highlightKeyword(list.get(startAt));
    if (list.size() <= startAt + 1) {
      analyzer.annotations.highlightError(list.get(startAt), "Conditional form missing");
      return 0;
    }
    analyzer.analyzeForm(list.get(startAt + 1));
    int consumed = 2;

    consumed += selectableRaw(analyzer, form, startAt + consumed);
    if (consumed == 0) return 0;

    // TODO: else
    if (list.size() <= startAt + consumed) return consumed;
    LispSexp sexp = list.get(startAt + consumed);
    if (sexp.getSymbol() != null && sexp.getSymbol().getText().equals("else")) {
      consumed++;
      int consumedElse = selectableRaw(analyzer, form, startAt + consumed);
      if (consumedElse == 0) return 0;
      return consumed + consumedElse;
    }

    return consumed;
  }

  private int selectableRaw(SyntaxAnalyzer analyzer, LispList form, int startAt) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() <= startAt) return 0;
    LispSexp sexp = list.get(startAt);
    if (sexp.getSymbol() == null) {
      analyzer.annotations.highlightError(sexp, "Loop keyword expected");
      return 0;
    }
    switch (sexp.getSymbol().getText()) {
      case "do":
      case "doing":
        return doDoingRaw(analyzer, form, startAt);
      case "return":
        return returnRaw(analyzer, form, startAt);
      case "append":
      case "appending":
      case "collect":
      case "collecting":
      case "count":
      case "counting":
      case "maximize":
      case "maximizing":
      case "minimize":
      case "minimizing":
      case "nconc":
      case "nconcing":
      case "sum":
      case "summing":
        return accumulationRaw(analyzer, form, startAt);
      case "if":
      case "when":
      case "unless":
        return conditionalRaw(analyzer, form, startAt);
      default:
        analyzer.annotations.highlightError(sexp, "Loop keyword expected");
        return 0;
    }
  }

  private void termination(SyntaxAnalyzer analyzer, LispList form, int startAt) {
    List<LispSexp> list = form.getSexpList();
    analyzer.annotations.highlightKeyword(list.get(startAt));
    if (list.size() <= startAt + 1) {
      analyzer.annotations.highlightError(list.get(startAt), "Termination form missing");
      return;
    }
    analyzer.analyzeForm(list.get(startAt + 1));
    mainClause(analyzer, form, startAt + 2);
  }

  private void aReturn(SyntaxAnalyzer analyzer, LispList form, int startAt) {
    if (returnRaw(analyzer, form, startAt) == 0) return;
    mainClause(analyzer, form, startAt + 2);
  }

  private int returnRaw(SyntaxAnalyzer analyzer, LispList form, int startAt) {
    List<LispSexp> list = form.getSexpList();
    analyzer.annotations.highlightKeyword(list.get(startAt));
    if (list.size() <= startAt + 1) {
      analyzer.annotations.highlightError(list.get(startAt - 1), "Return expression missing");
      return 0;
    }
    LispSexp arg = list.get(startAt + 1);
    LispSymbol symbol = arg.getSymbol();
    if (symbol != null && symbol.getText().equals("it")) {
      analyzer.annotations.highlightKeyword(arg);
    } else {
      analyzer.analyzeForm(arg);
    }
    return 2;
  }

  private void accumulation(SyntaxAnalyzer analyzer, LispList form, int startAt) {
    int consumed = accumulationRaw(analyzer, form, startAt);
    if (consumed == 0) return;
    mainClause(analyzer, form, startAt + consumed);
  }

  private int accumulationRaw(SyntaxAnalyzer analyzer, LispList form, int startAt) {
    List<LispSexp> list = form.getSexpList();
    analyzer.annotations.highlightKeyword(list.get(startAt));
    if (list.size() <= startAt + 1) {
      analyzer.annotations.highlightError(list.get(startAt - 1), "Accumulation expression missing");
      return 0;
    }
    LispSexp arg = list.get(startAt + 1);
    LispSymbol symbol = arg.getSymbol();
    if (symbol != null && symbol.getText().equals("it")) {
      analyzer.annotations.highlightKeyword(arg);
    } else {
      analyzer.analyzeForm(arg);
    }
    int consumed = 2;

    if (list.size() <= startAt + consumed) return consumed;
    LispSexp arg2 = list.get(startAt + consumed);
    LispSymbol symbol2 = arg2.getSymbol();
    if (symbol2 != null && symbol2.getText().equals("into")) {
      analyzer.annotations.highlightKeyword(arg2);
      consumed++;
      if (list.size() <= startAt + consumed) {
        analyzer.annotations.highlightError(arg2, "Accumulation destination missing");
        return 0;
      }
      LispSexp arg3 = list.get(startAt + consumed);
      LispSymbol symbol3 = arg3.getSymbol();
      if (symbol3 == null) {
        analyzer.annotations.highlightError(arg3, "Accumulation destination expected");
        return 0;
      }
      consumed++;
      // We don't support lexical bindings created by accumulations, because their scope is the whole loop.
      analyzer.lexicalBindings.defineLexicalVariables(form, List.of(symbol3)).close();
    }
    return consumed;
  }
}
