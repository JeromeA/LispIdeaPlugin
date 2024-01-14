package org.ax1.lisp.analysis;

import org.ax1.lisp.AnnotatedElement;

public interface BaseLispElement extends AnnotatedElement {

  void setErrorMessage(String errorMessage);
  void clear();
  Type getType();
  void setType(Type type);

  enum Type {
    UNKNOWN,
    COMMENT,
    DATA,
    CODE,
    ERROR,
    KEYWORD,
    PACKAGE_DEFINITION,
    PACKAGE_USAGE,
    CONDITION_DEFINITION,
    CONDITION_USAGE,
    FUNCTION_DEFINITION,
    FUNCTION_USAGE,
    SYMBOL_USAGE,
    METHOD_DEFINITION,
    VARIABLE_DEFINITION,
    VARIABLE_USAGE,
    LEXICAL_VARIABLE_DEFINITION,
    LEXICAL_VARIABLE_USAGE,
    LEXICAL_FUNCTION_DEFINITION,
    LEXICAL_FUNCTION_USAGE
  }
}
