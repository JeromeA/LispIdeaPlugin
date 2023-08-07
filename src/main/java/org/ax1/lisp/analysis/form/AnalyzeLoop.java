package org.ax1.lisp.analysis.form;

import com.google.common.collect.ImmutableSet;
import org.ax1.lisp.loop.LoopParser;
import org.ax1.lisp.loop.ParseException;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;

import java.io.ByteArrayInputStream;
import java.util.List;
import java.util.stream.Collectors;

import static org.ax1.lisp.analysis.BaseLispElement.Type.KEYWORD;

/**
 * <a href="http://www.lispworks.com/documentation/lw51/CLHS/Body/m_loop.htm">Reference</a>
 */
public class AnalyzeLoop implements FormAnalyzer {

  public static final ImmutableSet<String> LOOP_KEYWORDS = ImmutableSet.of("=", "ABOVE", "ACROSS", "ALWAYS", "AND",
      "APPEND", "APPENDING", "AS", "BEING", "BELOW", "BY", "COLLECT", "COLLECTING", "COUNT", "COUNTING", "DO", "DOING",
      "DOWNFROM", "DOWNTO", "EACH", "ELSE", "END", "EXTERNAL-SYMBOL", "EXTERNAL-SYMBOLS", "FINALLY", "FIXNUM", "FLOAT",
      "FOR", "FROM", "HASH-KEY", "HASH-KEYS", "HASH-VALUE", "HASH-VALUES", "IF", "IN", "INITIALLY", "INTO", "IT",
      "LOOP", "MAXIMIZE", "MAXIMIZING", "MINIMIZE", "MINIMIZING", "NAMED", "NCONC", "NCONCING", "NEVER", "NIL", "OF",
      "OF-TYPE", "ON", "PRESENT-SYMBOL", "PRESENT-SYMBOLS", "REPEAT", "RETURN", "SUM", "SUMMING", "SYMBOL", "SYMBOLS",
      "T", "THE", "THEN", "THEREIS", "TO", "UNLESS", "UNTIL", "USING", "UPFROM", "UPTO", "WHEN", "WHILE", "WITH");

  @Override
  public void analyze(LispList form) {
    List<LispSexp> sexpList = form.getSexpList();
    sexpList.get(0).setType(KEYWORD);
    String syntaxString = sexpList.stream().map(AnalyzeLoop::toSyntaxString).collect(Collectors.joining(" "));
    ByteArrayInputStream syntaxInput = new ByteArrayInputStream(syntaxString.getBytes());
    LoopParser parser = new LoopParser(syntaxInput);

    try {
      parser.Start(form);
    } catch (ParseException e) {
      System.err.println("Error: " + e.getMessage());
      System.err.println("In file: " + form.getContainingFile().getName());
      System.err.println("Original: " + form.getText());
      System.err.println("Modified: " + syntaxString);
//      e.printStackTrace();
    } finally {
      parser.End();
    }
  }

  private static String toSyntaxString(LispSexp sexp) {
    LispList list = sexp.getList();
    if (list != null) {
      return "()";
    }
    LispSymbol symbol = sexp.getSymbol();
    if (symbol != null) {
      String symbolName = symbol.getSymbolName().getValue();
      if (LOOP_KEYWORDS.contains(symbolName)) return symbolName;
    }
    return "X";
  }
 }
