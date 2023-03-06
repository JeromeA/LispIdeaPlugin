package org.ax1.lisp.subprocess;

import com.intellij.openapi.components.Service;
import com.intellij.openapi.project.Project;
import org.ax1.lisp.psi.*;
import org.ax1.lisp.subprocess.interaction.Interaction;
import org.jetbrains.annotations.NotNull;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static com.intellij.openapi.components.Service.Level.PROJECT;

/** Evaluate feature expressions, using the list of valid features from the Lisp subprocess. */
@Service(PROJECT)
public final class SubprocessFeatures {

  private static final Pattern FEATURE_PATTERN = Pattern.compile("[^()\\s]+");

  private final Project project;
  private Set<String> features;

  public static SubprocessFeatures getInstance(Project project) {
    return project.getService(SubprocessFeatures.class);
  }

  public SubprocessFeatures(Project project) {
    this.project = project;
  }

  public List<LispSexp> filterOptionalSexpList(@NotNull List<LispOptionalSexp> optionalSexpList) {
    return optionalSexpList.stream()
        .filter(this::isValidSexp)
        .map(LispOptionalSexp::getSexp)
        .collect(Collectors.toUnmodifiableList());
  }

  private boolean isValidSexp(LispOptionalSexp optionalSexp) {
    LispFeatureExp featureExp = optionalSexp.getFeatureExp();
    return featureExp == null || this.eval(featureExp);
  }

  public boolean eval(LispFeatureExp featureExp) {
    boolean positive = isPositive(featureExp);
    boolean featureValue = featureExp.getSimpleFeatureExp() != null ?
        eval(featureExp.getSimpleFeatureExp()) : eval(featureExp.getCompoundFeatureExp());
    return positive == featureValue;
  }

  private boolean eval(LispSimpleFeatureExp simpleFeatureExp) {
    return eval(simpleFeatureExp.getSymbolName());
  }

  private boolean eval(@NotNull LispSymbolName symbolName) {
    return getFeatures().contains(symbolName.getValue());
  }

  private boolean eval(LispCompoundFeatureExp compoundFeatureExp) {
    List<LispSymbolName> symbolNameList = compoundFeatureExp.getSymbolNameList();
    if (symbolNameList.isEmpty()) return false;
    switch (symbolNameList.get(0).getValue()) {
      case "NOT":
        if (symbolNameList.size() != 2) return false;
        return !eval(symbolNameList.get(1));
      case "OR":
        return symbolNameList.stream().skip(1).anyMatch(this::eval);
      case "AND":
        return symbolNameList.stream().skip(1).allMatch(this::eval);
    }
    return false;
  }

  private boolean isPositive(LispFeatureExp featureExp) {
    return featureExp.getText().startsWith("#+");
  }

  public synchronized Set<String> getFeatures() {
    if (features == null) {
      Set<String> newFeatures = new HashSet<>();
      Interaction interaction = LispServer.getInstance(project).evaluate("*features*", false);
      interaction.waitUntilComplete();
      String result = interaction.getResult();
      Matcher matcher = FEATURE_PATTERN.matcher(result);
      while(matcher.find()) {
        newFeatures.add(matcher.group());
      }
      features = newFeatures;
    }
    return features;
  }
}
