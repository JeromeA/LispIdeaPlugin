package org.ax1.lisp;

import com.intellij.lang.Commenter;
import org.jetbrains.annotations.Nullable;

public class LispCommenter implements Commenter {

  @Override
  public @Nullable String getLineCommentPrefix() {
    return ";;;";
  }

  @Override
  public @Nullable String getBlockCommentPrefix() {
    return null;
  }

  @Override
  public @Nullable String getBlockCommentSuffix() {
    return null;
  }

  @Override
  public @Nullable String getCommentedBlockCommentPrefix() {
    return null;
  }

  @Override
  public @Nullable String getCommentedBlockCommentSuffix() {
    return null;
  }
}
