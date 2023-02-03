package org.ax1.lisp.subprocess;

import com.intellij.codeInsight.daemon.DaemonCodeAnalyzer;
import com.intellij.openapi.application.PathManager;
import com.intellij.openapi.components.Service;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.SimpleModificationTracker;
import org.ax1.lisp.analysis.Bindings;
import org.ax1.lisp.analysis.symbol.CommonLispPackage;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.ax1.lisp.subprocess.interaction.Interaction;
import org.jetbrains.annotations.NotNull;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.intellij.lang.documentation.DocumentationMarkup.*;
import static com.intellij.openapi.components.Service.Level.PROJECT;
import static org.ax1.lisp.analysis.symbol.SymbolDefinition.newDefinition;

@Service(PROJECT)
public final class ExternalDocumentation {

  private static final Pattern describePattern =
      Pattern.compile("^[^\n]+ names a (compiled function|macro):.*?\n\n", Pattern.MULTILINE | Pattern.DOTALL);
  private static final Pattern namePattern = Pattern.compile("^([^\n]+) names a (compiled function|macro):.*", Pattern.DOTALL);
  private static final Pattern docPattern =
      Pattern.compile(".*\n  Documentation:\n(.*?)\n(  [^ ]|\n).*", Pattern.DOTALL);
  private static final Pattern lambdaPattern =
      Pattern.compile(".*\n  Lambda-list: (.*?)\n(  [^ ]).*", Pattern.DOTALL);

  private Bindings bindings;
  private final Project project;
  private final SimpleModificationTracker modificationTracker = new SimpleModificationTracker();

  public static ExternalDocumentation getInstance(Project project) {
    return project.getService(ExternalDocumentation.class);
  }

  public ExternalDocumentation(Project project) {
    this.project = project;
  }

  public SimpleModificationTracker getModificationTracker() {
    return modificationTracker;
  }

  @NotNull
  private Path getDocumentationPath() {
    return Paths.get(PathManager.getSystemPath(), "lisp", "documentation");
  }

  public Bindings getBindings() {
    if (bindings == null) readFromCache();
    return bindings;
  }

  private void readFromCache() {
    // TODO: Read cache from PathManager.getSystemPath()/lisp/doc
    Path documentationPath = getDocumentationPath();
    bindings = new Bindings();
  }

  private void writeToCache() {
    // TODO: Write to cache in PathManager.getSystemPath()/lisp/doc
  }

  public void updateDocumentation() {
    Interaction interaction = LispServer.getInstance(project).evaluate("(lisp-idea-plugin:get-documentation)", false);
    interaction.waitUntilComplete();
    Bindings newBindings = new Bindings();
    Matcher matcher = describePattern.matcher(interaction.getStdout());
    while(matcher.find()) {
      String singleFunctionBlock = matcher.group();
      Matcher nameMatcher = namePattern.matcher(singleFunctionBlock);
      if (!nameMatcher.matches()) {
        throw new RuntimeException("Name not found");
      }
      String type = nameMatcher.group(2);
      Symbol symbol = CommonLispPackage.INSTANCE.intern(nameMatcher.group(1).toUpperCase());
      SymbolDefinition symbolDefinition = newDefinition(SymbolDefinition.Type.FUNCTION, SymbolDefinition.Scope.DYNAMIC, symbol);
      Matcher docMatcher = docPattern.matcher(singleFunctionBlock);
      String doc = docMatcher.matches() ? docMatcher.group(1) : null;
      Matcher lambdaMatcher = lambdaPattern.matcher(singleFunctionBlock);
      if (!lambdaMatcher.matches()) {
        throw new RuntimeException("Lambda no found");
      }
      String lambda = lambdaMatcher.group(1);
      symbolDefinition.setLambda(lambda);
      symbolDefinition.hasExternalDefinition = true;
      symbolDefinition.setDescription(getDescription(type.equals("compiled function") ? "Function" : "Macro", symbol, lambda, doc));
      newBindings.addDefinition(symbolDefinition);
    }
    bindings = newBindings;
    modificationTracker.incModificationCount();
    DaemonCodeAnalyzer.getInstance(project).restart();
  }

  private static String getDescription(String type, Symbol symbol, String lambda, String documentation) {
      StringBuilder sb = new StringBuilder();
      sb.append(DEFINITION_ELEMENT.addText(type + " " + symbol.getQualifiedName()));
      sb.append(SECTIONS_START);
      sb.append(SECTION_HEADER_CELL.addText("Lambda:"));
      sb.append(SECTION_CONTENT_CELL.addText(lambda));
      sb.append("</tr>");
      sb.append(SECTION_HEADER_CELL.addText("Documentation:"));
      sb.append(SECTION_CONTENT_CELL.addText(documentation == null ? "--" : documentation));
      sb.append(SECTIONS_END);
      return sb.toString();
  }
}
