package org.ax1.lisp;

import com.intellij.codeInsight.daemon.*;
import com.intellij.icons.AllIcons;
import com.intellij.openapi.editor.markup.GutterIconRenderer;
import com.intellij.psi.NavigatablePsiElement;
import com.intellij.psi.PsiElement;
import com.intellij.psi.impl.source.tree.LeafPsiElement;
import com.intellij.psi.tree.IElementType;
import org.ax1.lisp.analysis.ProjectData;
import org.ax1.lisp.psi.LispSymbolName;
import org.ax1.lisp.psi.LispTypes;
import org.ax1.lisp.psi.impl.LispStringDesignator;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.stream.Collectors;

import static org.ax1.lisp.LispIcons.TO_GENERIC;
import static org.ax1.lisp.LispIcons.TO_METHOD;

public class LispLineMarkerProvider extends LineMarkerProviderDescriptor {

  private final GutterIconDescriptor.Option implementedOption = new GutterIconDescriptor.Option("lisp.implemented", "Implemented generic", AllIcons.Gutter.OverridenMethod);
  private final GutterIconDescriptor.Option implementingOption = new GutterIconDescriptor.Option("lisp.implementing", "Implementing method", AllIcons.Gutter.OverridingMethod);

  @Override
  public @GutterName String getName() {
    return "Lisp line markers";
  }

  @Override
  public LineMarkerInfo<?> getLineMarkerInfo(@NotNull PsiElement element) {
    if (getTokenType(element) != LispTypes.SYMBOL_TOKEN) return null;
    if (!(element.getParent() instanceof LispSymbolName)) return null;
    LispSymbolName symbolName = (LispSymbolName) element.getParent();
    if (symbolName.getType() == LispStringDesignator.Type.METHOD_DEFINITION) {
      LispStringDesignator functionDefinition = ProjectData.getInstance(element.getProject()).getFunctionDefinition(symbolName);
      if (functionDefinition == null) return null;
      return new LineMarkerInfo<>(element, element.getTextRange(), TO_GENERIC,
          null, new DefaultGutterIconNavigationHandler<>(List.of((NavigatablePsiElement) functionDefinition), "Generic Definition"),
          GutterIconRenderer.Alignment.RIGHT, () -> "Go to generic");
    }
    if (symbolName.getType() == LispStringDesignator.Type.FUNCTION_DEFINITION) {
      List<NavigatablePsiElement> targets =
          ProjectData.getInstance(element.getProject()).getMethodDefinitions(symbolName).stream()
              .map(NavigatablePsiElement.class::cast)
              .collect(Collectors.toUnmodifiableList());
      if (targets.isEmpty()) return null;
      return new LineMarkerInfo<>(element, element.getTextRange(), TO_METHOD,
          null, new DefaultGutterIconNavigationHandler<>(targets, "Implementations"),
          GutterIconRenderer.Alignment.RIGHT, () -> "Go to implementations");
    }
    return null;
  }

  private IElementType getTokenType(PsiElement element) {
    if (element instanceof LeafPsiElement) {
      return ((LeafPsiElement) element).getElementType();
    }
    return null;
  }

  @Override
  public Option @NotNull [] getOptions() {
    return new Option[]{implementedOption, implementingOption};
  }
}
