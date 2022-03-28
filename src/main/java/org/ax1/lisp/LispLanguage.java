package org.ax1.lisp;

import com.intellij.lang.Language;

public class LispLanguage extends Language {
  public static final LispLanguage INSTANCE = new LispLanguage();

  protected LispLanguage() {
    super("Lisp");
  }
}
