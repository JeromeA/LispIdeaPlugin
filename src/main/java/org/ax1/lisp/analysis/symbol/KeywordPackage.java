package org.ax1.lisp.analysis.symbol;

public class KeywordPackage extends LispPackage {

  public static final String KEYWORD = "KEYWORD";

  public static KeywordPackage INSTANCE = new KeywordPackage();

  private KeywordPackage() {
    super(createDefinition());
  }

  private static PackageDefinition createDefinition() {
    PackageDefinition definition = new PackageDefinition(KEYWORD);
    definition.setReadOnly();
    return definition;
  }
}
