package org.ax1.lisp.analysis.symbol;

public class CommonLispUserPackage extends LispPackage {

  public CommonLispUserPackage(PackageManager packageManager) {
    super(packageManager, createDefinition());
  }

  private static PackageDefinition createDefinition() {
    PackageDefinition definition = new PackageDefinition("COMMON-LISP-USER");
    definition.addNickname("CL-USER");
    definition.use.put(CommonLispPackage.COMMON_LISP, null);
    return definition;
  }
}
