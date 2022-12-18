package org.ax1.lisp.analysis.symbol;

public class CommonLispUserPackage extends LispPackage {

  public static final String COMMON_LISP_USER = "COMMON-LISP-USER";

  public CommonLispUserPackage(PackageManager packageManager) {
    super(packageManager, createDefinition());
  }

  private static PackageDefinition createDefinition() {
    PackageDefinition definition = new PackageDefinition(COMMON_LISP_USER);
    definition.addNickname("CL-USER");
    definition.use.put(CommonLispPackage.COMMON_LISP, null);
    return definition;
  }
}
