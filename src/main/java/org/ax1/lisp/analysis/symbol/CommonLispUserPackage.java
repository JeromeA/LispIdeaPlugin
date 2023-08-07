package org.ax1.lisp.analysis.symbol;

public class CommonLispUserPackage extends Package {

  public static final String COMMON_LISP_USER = "COMMON-LISP-USER";
  public static final CommonLispUserPackage INSTANCE = new CommonLispUserPackage();

  public CommonLispUserPackage() {
    super(COMMON_LISP_USER);
    addNickname("CL-USER");
    addUse(CommonLispPackage.COMMON_LISP);
  }
}
