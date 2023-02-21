package org.ax1.lisp.subprocess;

import com.intellij.codeInsight.daemon.DaemonCodeAnalyzer;
import com.intellij.openapi.components.Service;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.SimpleModificationTracker;
import org.ax1.lisp.analysis.Bindings;
import org.ax1.lisp.analysis.symbol.CommonLispPackage;
import org.ax1.lisp.analysis.symbol.Lambda;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.ax1.lisp.subprocess.interaction.Interaction;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.intellij.openapi.components.Service.Level.PROJECT;
import static org.ax1.lisp.analysis.symbol.SymbolDefinition.newDefinition;

@Service(PROJECT)
public final class ExternalDocumentation {

  private static final Pattern describePattern =
      Pattern.compile("^[^\n]+ names a (compiled function|special operator|macro):.*?\n\n", Pattern.MULTILINE | Pattern.DOTALL);
  private static final Pattern namePattern = Pattern.compile("^([^\n]+) names a (compiled function|special operator|macro):.*", Pattern.DOTALL);
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

  public Bindings getBindings() {
    if (bindings == null) updateDocumentation();
    return bindings;
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
      String name = nameMatcher.group(1).toUpperCase();
      if (name.startsWith("(SETF")) continue;
      Symbol symbol = CommonLispPackage.INSTANCE.intern(name);
      SymbolDefinition symbolDefinition = newDefinition(toType(type), SymbolDefinition.Scope.DYNAMIC, symbol);
      Matcher docMatcher = docPattern.matcher(singleFunctionBlock);
      if (docMatcher.matches()) {
        symbolDefinition.setDescriptionString(docMatcher.group(1));
      }
      Matcher lambdaMatcher = lambdaPattern.matcher(singleFunctionBlock);
      if (!lambdaMatcher.matches()) {
        throw new RuntimeException("Lambda no found");
      }
      String lambda = lambdaMatcher.group(1);
      symbolDefinition.setLambda(Lambda.from(project, lambda));
      symbolDefinition.hasExternalDefinition = true;
      newBindings.addDefinition(symbolDefinition);
    }
    bindings = newBindings;
    modificationTracker.incModificationCount();
    DaemonCodeAnalyzer.getInstance(project).restart();
  }

  private static SymbolDefinition.Type toType(String type) {
    switch (type) {
      case "compiled function": return SymbolDefinition.Type.FUNCTION;
      case "macro": return SymbolDefinition.Type.MACRO;
      case "special operator": return SymbolDefinition.Type.SPECIAL_OPERATOR;
      default:
        throw new IllegalStateException("Unexpected value: " + type);
    }
  }
}
