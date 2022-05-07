package org.ax1.lisp.analysis.symbol;

import java.util.Set;

public class CommonLispUserPackage extends LispPackage {

  public CommonLispUserPackage() {
    super("COMMON-LISP-USER");
    setNicknames(Set.of("CL-USER"));
    addUse("COMMON-LISP");
  }
}
