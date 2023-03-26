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

  private static final Pattern DESCRIBE_PATTERN =
      Pattern.compile("^[^\n]+ names a (compiled function|special operator|macro|special variable):.*?\n\n", Pattern.MULTILINE | Pattern.DOTALL);
  private static final Pattern NAME_PATTERN =
      Pattern.compile("^([^\n]+) names a (compiled function|special operator|macro|special variable):.*", Pattern.DOTALL);
  private static final Pattern DOC_PATTERN =
      Pattern.compile(".*\n  Documentation:\n(.*?)\n(  [^ ]|\n).*", Pattern.DOTALL);
  private static final Pattern LAMBDA_PATTERN =
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

  public synchronized Bindings getBindings() {
    if (bindings == null) updateDocumentation();
    return bindings;
  }

  private void updateDocumentation() {
    Interaction interaction = LispServer.getInstance(project).evaluate("(lisp-idea-plugin:get-documentation)", false);
    interaction.waitUntilComplete();
    Bindings newBindings = new Bindings();
    Matcher matcher = DESCRIBE_PATTERN.matcher(interaction.getStdout());
    while(matcher.find()) {
      String singleFunctionBlock = matcher.group();
      Matcher nameMatcher = NAME_PATTERN.matcher(singleFunctionBlock);
      if (!nameMatcher.matches()) {
        throw new RuntimeException("Name not found");
      }
      SymbolDefinition.Type type = toType(nameMatcher.group(2));
      String name = nameMatcher.group(1).toUpperCase();
      if (name.startsWith("(SETF")) continue;
      Symbol symbol = CommonLispPackage.INSTANCE.intern(name);
      SymbolDefinition symbolDefinition = newDefinition(type, SymbolDefinition.Scope.DYNAMIC, symbol);
      Matcher docMatcher = DOC_PATTERN.matcher(singleFunctionBlock);
      if (docMatcher.matches()) {
        symbolDefinition.setDescriptionString(docMatcher.group(1));
      }
      if (type != SymbolDefinition.Type.VARIABLE) {
        Matcher lambdaMatcher = LAMBDA_PATTERN.matcher(singleFunctionBlock);
        if (!lambdaMatcher.matches()) {
          throw new RuntimeException("Lambda not found");
        }
        String lambda = lambdaMatcher.group(1);
        symbolDefinition.setLambda(Lambda.from(project, lambda));
      }
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
      case "special variable": return SymbolDefinition.Type.VARIABLE;
      default:
        throw new IllegalStateException("Unexpected value: " + type);
    }
  }
}
