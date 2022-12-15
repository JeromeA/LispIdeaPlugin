package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.analysis.LexicalBindingManager.LexicalScope;
import org.ax1.lisp.analysis.LexicalVariableHelper;
import org.ax1.lisp.analysis.LocatedSymbol;
import org.ax1.lisp.analysis.SymbolBinding;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static com.google.common.collect.ImmutableList.toImmutableList;

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
  public void analyze(AnalysisContext context, LispList form) {
    context.highlighter.highlightKeyword(form.getSexpList().get(0));
    if (form.getSexpList().size() == 1) return;
    LispSexp firstSexp = form.getSexpList().get(1);
    if (firstSexp.getList() != null) {
      context.analyzer.analyzeForms(form.getSexpList(), 1);
    } else {
      nameClause(context, form);
    }
  }

  /**
   * Parse an optional name clause (named name), on continue with variable clauses.
   */
  private void nameClause(AnalysisContext context, LispList form) {
    List<LispSexp> list = form.getSexpList();
    LispSymbol symbol = list.get(1).getSymbol();
    if (symbol == null) {
      context.highlighter.highlightError(list.get(1), "Loop keyword expected");
      return;
    }
    if (symbol.getText().equals("named")) {
      if (list.size() <= 2) {
        context.highlighter.highlightError(form, "Name missing");
        return;
      }
      if (list.get(2).getSymbol() == null) {
        context.highlighter.highlightError(list.get(2), "Name expected");
        return;
      }
      variableClause(context, form, 3);
    }
    variableClause(context, form, 1);
  }

  /**
   * Parse optional variable clauses, and continue with main clauses.
   */
  private void variableClause(AnalysisContext context, LispList form, int startAt) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() <= startAt) return;
    LispSymbol symbol = list.get(startAt).getSymbol();
    if (symbol == null) {
      context.highlighter.highlightError(list.get(startAt), "Loop keyword expected");
      return;
    }
    switch (symbol.getText()) {
      case "with":
        with(context, form, startAt);
        break;
      case "initially":
      case "finally":
        initiallyFinally(context, form, startAt, false);
        break;
      case "for":
      case "as":
        forAs(context, form, startAt);
        break;
      default:
        mainClause(context, form, startAt);
        break;
    }
  }

  private void initiallyFinally(AnalysisContext context, LispList form, int startAt, boolean inMainClause) {
    List<LispSexp> list = form.getSexpList();
    context.highlighter.highlightKeyword(list.get(startAt));
    startAt++;
    if (list.size() <= startAt) {
      context.highlighter.highlightError(list.get(startAt - 1), "Form missing");
      return;
    }
    while (list.size() > startAt && list.get(startAt).getList() != null) {
      context.analyzer.analyzeForm(list.get(startAt));
      startAt++;
    }
    if (inMainClause) {
      mainClause(context, form, startAt);
    } else {
      variableClause(context, form, startAt);
    }
  }

  private void doDoing(AnalysisContext context, LispList form, int startAt) {
    int consumed = doDoingRaw(context, form, startAt);
    if (consumed == 0) return;
    mainClause(context, form, startAt + consumed);
  }

  /**
   * Parse a do/doing statement. Such a statement can have any number of compound forms to execute.
   */
  private int doDoingRaw(AnalysisContext context, LispList form, int startAt) {
    List<LispSexp> list = form.getSexpList();
    context.highlighter.highlightKeyword(list.get(startAt));
    int consumed = 1;
    if (list.size() <= startAt + consumed) {
      context.highlighter.highlightError(list.get(startAt - 1), "Form missing");
      return 0;
    }
    while (list.size() > startAt + consumed && list.get(startAt + consumed).getList() != null) {
      context.analyzer.analyzeForm(list.get(startAt + consumed));
      consumed++;
    }
    return consumed;
  }

  private void with(AnalysisContext context, LispList form, int startAt) {
    List<LispSexp> list = form.getSexpList();
    context.highlighter.highlightKeyword(list.get(startAt));
    startAt++;

    if (list.size() <= startAt) {
      context.highlighter.highlightError(list.get(startAt - 1), "Variable missing");
      return;
    }
    LispSexp sexp1 = list.get(startAt);
    if (sexp1.getSymbol() == null) {
      context.highlighter.highlightError(sexp1, "Variable name expected");
      return;
    }
    startAt++;

    if (list.size() <= startAt) return;
    LispSexp sexp2 = list.get(startAt);
    if (sexp2.getSymbol() == null) {
      context.highlighter.highlightError(sexp2, "Loop keyword expected");
      return;
    }
    if (sexp2.getSymbol().getText().equals("=")) {
      context.highlighter.highlightKeyword(sexp2);
      context.analyzer.analyzeForm(list.get(startAt + 1));
      startAt += 2;
    }

    SymbolBinding variable = LexicalVariableHelper.newLexicalVariable("LOOP",
        context.packageManager.getLocatedSymbol(sexp1.getSymbol()), null);
    try (LexicalScope ignored = context.lexicalBindings.defineLexicalVariables(List.of(variable))) {
      variableClause(context, form, startAt);
    }
  }

  private void forAs(AnalysisContext context, LispList form, int startAt) {
    List<LispSexp> list = form.getSexpList();
    context.highlighter.highlightKeyword(list.get(startAt));
    startAt++;

    if (list.size() <= startAt) {
      context.highlighter.highlightError(list.get(startAt - 1), "Variable name missing");
      return;
    }
    LispSexp sexp1 = list.get(startAt);
    List<LispSymbol> variables = getVariables(context, sexp1);
    startAt++;

    while (true) {
      if (list.size() <= startAt) return;
      LispSexp sexp2 = list.get(startAt);
      if (sexp2.getSymbol() == null) {
        context.highlighter.highlightError(sexp2, "Loop keyword expected");
        return;
      }
      if (!FOR_SUBCLAUSE_KEYWORDS.contains(sexp2.getSymbol().getText())) break;
      context.highlighter.highlightKeyword(sexp2);
      context.analyzer.analyzeForm(list.get(startAt + 1));
      startAt += 2;
    }

    List<SymbolBinding> locatedVariables = variables.stream()
        .map(context.packageManager::getLocatedSymbol)
        .map(locatedSymbol -> LexicalVariableHelper.newLexicalVariable("LOOP", locatedSymbol, null))
        .collect(toImmutableList());
    try (LexicalScope ignored = context.lexicalBindings.defineLexicalVariables(locatedVariables)) {
      variableClause(context, form, startAt);
    }
  }

  private List<LispSymbol> getVariables(AnalysisContext context, LispSexp sexp) {
    LispSymbol symbol = sexp.getSymbol();
    if (symbol != null) {
      if (symbol.getText().equals("nil")) {
        context.highlighter.highlightKeyword(symbol);
        return List.of();
      }
      return List.of(symbol);
    }
    LispList list = sexp.getList();
    if (list != null) {
      List<LispSexp> sexpList = list.getSexpList();
      return sexpList.stream().flatMap(s -> getVariables(context, s).stream()).collect(Collectors.toList());
    }
    context.highlighter.highlightError(sexp, "Variable name expected");
    return List.of();
  }


  private void mainClause(AnalysisContext context, LispList form, int startAt) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() <= startAt) return;
    LispSexp sexp = list.get(startAt);
    if (sexp.getSymbol() == null) {
      context.highlighter.highlightError(sexp, "Loop keyword expected");
      return;
    }
    switch (sexp.getSymbol().getText()) {
      case "do":
      case "doing":
        doDoing(context, form, startAt);
        break;
      case "return":
        aReturn(context, form, startAt);
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
        accumulation(context, form, startAt);
        break;
      case "initially":
      case "finally":
        initiallyFinally(context, form, startAt, true);
        break;
      case "always":
      case "never":
      case "repeat":
      case "thereis":
      case "until":
      case "while":
        termination(context, form, startAt);
        break;
      case "if":
      case "when":
      case "unless":
        conditional(context, form, startAt);
        break;
      default:
        context.highlighter.highlightError(sexp, "Loop keyword expected");
    }
  }

  private void conditional(AnalysisContext context, LispList form, int startAt) {
    int consumed = conditionalRaw(context, form, startAt);
    if (consumed == 0) return;
    mainClause(context, form, startAt + consumed);
  }

  private int conditionalRaw(AnalysisContext context, LispList form, int startAt) {
    List<LispSexp> list = form.getSexpList();
    context.highlighter.highlightKeyword(list.get(startAt));
    if (list.size() <= startAt + 1) {
      context.highlighter.highlightError(list.get(startAt), "Conditional form missing");
      return 0;
    }
    context.analyzer.analyzeForm(list.get(startAt + 1));
    int consumed = 2;

    consumed += selectableRaw(context, form, startAt + consumed);
    if (consumed == 0) return 0;

    // TODO: else
    if (list.size() <= startAt + consumed) return consumed;
    LispSexp sexp = list.get(startAt + consumed);
    if (sexp.getSymbol() != null && sexp.getSymbol().getText().equals("else")) {
      consumed++;
      int consumedElse = selectableRaw(context, form, startAt + consumed);
      if (consumedElse == 0) return 0;
      return consumed + consumedElse;
    }

    return consumed;
  }

  private int selectableRaw(AnalysisContext context, LispList form, int startAt) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() <= startAt) return 0;
    LispSexp sexp = list.get(startAt);
    if (sexp.getSymbol() == null) {
      context.highlighter.highlightError(sexp, "Loop keyword expected");
      return 0;
    }
    switch (sexp.getSymbol().getText()) {
      case "do":
      case "doing":
        return doDoingRaw(context, form, startAt);
      case "return":
        return returnRaw(context, form, startAt);
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
        return accumulationRaw(context, form, startAt);
      case "if":
      case "when":
      case "unless":
        return conditionalRaw(context, form, startAt);
      default:
        context.highlighter.highlightError(sexp, "Loop keyword expected");
        return 0;
    }
  }

  private void termination(AnalysisContext context, LispList form, int startAt) {
    List<LispSexp> list = form.getSexpList();
    context.highlighter.highlightKeyword(list.get(startAt));
    if (list.size() <= startAt + 1) {
      context.highlighter.highlightError(list.get(startAt), "Termination form missing");
      return;
    }
    context.analyzer.analyzeForm(list.get(startAt + 1));
    mainClause(context, form, startAt + 2);
  }

  private void aReturn(AnalysisContext context, LispList form, int startAt) {
    if (returnRaw(context, form, startAt) == 0) return;
    mainClause(context, form, startAt + 2);
  }

  private int returnRaw(AnalysisContext context, LispList form, int startAt) {
    List<LispSexp> list = form.getSexpList();
    context.highlighter.highlightKeyword(list.get(startAt));
    if (list.size() <= startAt + 1) {
      context.highlighter.highlightError(list.get(startAt - 1), "Return expression missing");
      return 0;
    }
    LispSexp arg = list.get(startAt + 1);
    LispSymbol symbol = arg.getSymbol();
    if (symbol != null && symbol.getText().equals("it")) {
      context.highlighter.highlightKeyword(arg);
    } else {
      context.analyzer.analyzeForm(arg);
    }
    return 2;
  }

  private void accumulation(AnalysisContext context, LispList form, int startAt) {
    int consumed = accumulationRaw(context, form, startAt);
    if (consumed == 0) return;
    mainClause(context, form, startAt + consumed);
  }

  private int accumulationRaw(AnalysisContext context, LispList form, int startAt) {
    List<LispSexp> list = form.getSexpList();
    context.highlighter.highlightKeyword(list.get(startAt));
    if (list.size() <= startAt + 1) {
      context.highlighter.highlightError(list.get(startAt - 1), "Accumulation expression missing");
      return 0;
    }
    LispSexp arg = list.get(startAt + 1);
    LispSymbol symbol = arg.getSymbol();
    if (symbol != null && symbol.getText().equals("it")) {
      context.highlighter.highlightKeyword(arg);
    } else {
      context.analyzer.analyzeForm(arg);
    }
    int consumed = 2;

    if (list.size() <= startAt + consumed) return consumed;
    LispSexp arg2 = list.get(startAt + consumed);
    LispSymbol symbol2 = arg2.getSymbol();
    if (symbol2 != null && symbol2.getText().equals("into")) {
      context.highlighter.highlightKeyword(arg2);
      consumed++;
      if (list.size() <= startAt + consumed) {
        context.highlighter.highlightError(arg2, "Accumulation destination missing");
        return 0;
      }
      LispSexp arg3 = list.get(startAt + consumed);
      LispSymbol symbol3 = arg3.getSymbol();
      if (symbol3 == null) {
        context.highlighter.highlightError(arg3, "Accumulation destination expected");
        return 0;
      }
      consumed++;
      // We don't support lexical bindings created by accumulations, because their scope is the whole loop.
      SymbolBinding variable = LexicalVariableHelper.newLexicalVariable("LOOP",
          context.packageManager.getLocatedSymbol(symbol3), null);
      context.lexicalBindings.defineLexicalVariables(List.of(variable)).close();
    }
    return consumed;
  }
}
