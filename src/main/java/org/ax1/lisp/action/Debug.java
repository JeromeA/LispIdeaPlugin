package org.ax1.lisp.action;

import com.intellij.icons.AllIcons;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.psi.StubBasedPsiElement;
import com.intellij.psi.stubs.StubIndex;
import org.ax1.lisp.psi.LispSymbolName;
import org.ax1.lisp.psi.impl.LispStringDesignator;
import org.ax1.lisp.stubs.LispNameIndex;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.stream.Collectors;

public class Debug extends AnAction {
  public Debug() {
    super("Trigger Internal Test", "Start an internal test, for debugging purpose.", AllIcons.General.Settings);
  }

  @Override
  public void update(@NotNull AnActionEvent e) {
  }

  @Override
  public void actionPerformed(@NotNull AnActionEvent e) {
    System.err.println("Dumping index:");
    StubIndex index = StubIndex.getInstance();
    index.processAllKeys(LispNameIndex.LISP_NAMES, e.getProject(), key -> {
      System.err.println("Value: " + key);
      Collection<LispStringDesignator> readElements = StubIndex.getElements(LispNameIndex.LISP_NAMES, key, e.getProject(), null, LispStringDesignator.class);
      System.err.println("  -> " + readElements);
      System.err.println("  -> " + readElements.stream().map(v -> v.getClass()).collect(Collectors.toList()));
      System.err.println("  -> " + readElements.stream().map(v -> ((StubBasedPsiElement)v).getStub()).collect(Collectors.toList()));
      return true;
    });
  }

}
