package org.ax1.lisp.stubs;

import org.ax1.lisp.analysis.BaseLispElement;

public interface LispStringDesignatorStubInterface {

  String getPackageContext();
  String getLispName();
  BaseLispElement.Type getType();
}
