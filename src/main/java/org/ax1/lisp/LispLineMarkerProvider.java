package org.ax1.lisp;

import com.intellij.codeInsight.daemon.*;
import com.intellij.icons.AllIcons;
import com.intellij.openapi.editor.markup.GutterIconRenderer;
import com.intellij.psi.NavigatablePsiElement;
import com.intellij.psi.PsiElement;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.ax1.lisp.psi.LispSexp;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
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
    if (element instanceof LispSexp) {
      LispSexp sexp = (LispSexp) element;
      SymbolDefinition symbolDefinition = sexp.getSymbolDefinition();
      if (symbolDefinition == null) return null;
      if (symbolDefinition.type != SymbolDefinition.Type.FUNCTION) return null;
      if (symbolDefinition.methods.contains(sexp) && !symbolDefinition.getDefinitions().isEmpty()) {
        NavigatablePsiElement target = (NavigatablePsiElement) symbolDefinition.getDefinition();
        return new LineMarkerInfo<>(element, element.getTextRange(), TO_GENERIC,
            null, new DefaultGutterIconNavigationHandler<>(List.of(target), "Generic Definition"),
            GutterIconRenderer.Alignment.RIGHT, () -> "Go to generic");
      }
      if (symbolDefinition.isDefinition(sexp) && !symbolDefinition.methods.isEmpty()) {
        Collection<NavigatablePsiElement> targets = symbolDefinition.methods.stream()
            .map(NavigatablePsiElement.class::cast)
            .collect(Collectors.toList());
        return new LineMarkerInfo<>(element, element.getTextRange(), TO_METHOD,
            null, new DefaultGutterIconNavigationHandler<>(targets, "Implementations"),
            GutterIconRenderer.Alignment.RIGHT, () -> "Go to implementations");
      }
    }
    return null;
  }

  @Override
  public Option @NotNull [] getOptions() {
    return new Option[]{implementedOption, implementingOption};
  }
}
