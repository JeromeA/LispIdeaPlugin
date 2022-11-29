package org.ax1.lisp;

import com.intellij.codeInsight.completion.*;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.openapi.project.Project;
import com.intellij.patterns.PlatformPatterns;
import com.intellij.psi.PsiElement;
import com.intellij.util.ProcessingContext;
import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.analysis.ProjectComputedData;
import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.analysis.symbol.PackageManager;
import org.ax1.lisp.psi.LispFile;
import org.jetbrains.annotations.NotNull;

import static org.ax1.lisp.analysis.LispAnnotator.EMPTY_HIGHLIGHTER;
import static org.ax1.lisp.psi.LispTypes.SYMBOL_TOKEN;

public class LispCompletionContributor extends CompletionContributor {

  public LispCompletionContributor() {
    extend(CompletionType.BASIC, PlatformPatterns.psiElement(SYMBOL_TOKEN), new CompletionProvider<>() {
      @Override
      protected void addCompletions(@NotNull CompletionParameters parameters, @NotNull ProcessingContext context, @NotNull CompletionResultSet result) {
        PsiElement symbolToken = parameters.getPosition();
        Project project = symbolToken.getProject();
        ProjectComputedData projectComputedData = ProjectComputedData.getInstance(project);
        LispFile lispFile = (LispFile) symbolToken.getContainingFile();
        SyntaxAnalyzer syntaxAnalyzer = new SyntaxAnalyzer(lispFile);
        AnalysisContext analysisContext = new AnalysisContext(EMPTY_HIGHLIGHTER, new PackageManager(projectComputedData.getPackages()), syntaxAnalyzer);
        syntaxAnalyzer.setContext(analysisContext);
        syntaxAnalyzer.analyze();
        syntaxAnalyzer.completions.stream()
            .map(String::toLowerCase)
            .forEach(name -> result.addElement(LookupElementBuilder.create(name)));
      }
    });
  }

}
