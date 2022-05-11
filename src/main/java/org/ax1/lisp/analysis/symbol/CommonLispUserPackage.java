package org.ax1.lisp.analysis.symbol;

public class CommonLispUserPackage extends LispPackage {

  public CommonLispUserPackage() {
    super(createDefinition());
  }

  private static PackageDefinition createDefinition() {
    PackageDefinition definition = new PackageDefinition("COMMON-LISP-USER");
    definition.addNickname("CL-USER");
    definition.addUse("COMMON-LISP");
    definition.setStandard();
    return definition;
  }
}
