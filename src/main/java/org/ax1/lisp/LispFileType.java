package org.ax1.lisp;

import com.intellij.openapi.fileTypes.LanguageFileType;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class LispFileType extends LanguageFileType {

  public static final LispFileType INSTANCE = new LispFileType();

  protected LispFileType() {
    super(LispLanguage.INSTANCE);
  }

  @Override
  public @NonNls
  @NotNull String getName() {
    return "Lisp";
  }

  @Override
  public
  @NotNull String getDescription() {
    return "Lisp language file";
  }

  @Override
  public
  @NotNull String getDefaultExtension() {
    return "lisp";
  }

  @Override
  public @Nullable Icon getIcon() {
    return LispIcons.FILE;
  }
}
