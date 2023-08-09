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
import static org.ax1.lisp.analysis.BaseLispElement.Type.CODE;
import static org.ax1.lisp.analysis.BaseLispElement.Type.COMMENT;

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

  public List<LispSexp> filterOptionalSexpList(@NotNull List<LispPrefixedSexp> prefixedSexpList) {
    return prefixedSexpList.stream()
        .filter(this::isValidSexp)
        .map(LispPrefixedSexp::getSexp)
        .collect(Collectors.toUnmodifiableList());
  }

  public boolean isValidSexp(LispPrefixedSexp prefixedSexp) {
    @NotNull List<LispReaderFeature> readerFeatureList = prefixedSexp.getReaderFeatureList();
    if (readerFeatureList.isEmpty() || this.eval(readerFeatureList.get(0))) {
      return true;
    } else {
      prefixedSexp.getSexp().setType(COMMENT);
      return false;
    }
  }

  public boolean eval(LispReaderFeature readerFeature) {
    boolean positive = isPositive(readerFeature);
    boolean featureValue = eval(readerFeature.getFeatureExp());
    return positive == featureValue;
  }

  private boolean eval(LispFeatureExp featureExp) {
    if (featureExp.getSimpleFeatureExp() != null) return eval(featureExp.getSimpleFeatureExp());
    return eval(featureExp.getCompoundFeatureExp());
  }

  private boolean eval(LispSimpleFeatureExp simpleFeatureExp) {
    return eval(simpleFeatureExp.getSymbolName());
  }

  private boolean eval(@NotNull LispSymbolName symbolName) {
    boolean isValid = getFeatures().contains(symbolName.getValue());
    symbolName.setType(isValid ? CODE : COMMENT);
    return isValid;
  }

  private boolean eval(LispCompoundFeatureExp compoundFeatureExp) {
    @NotNull List<LispFeatureExp> featureExpList = compoundFeatureExp.getFeatureExpList();
    if (featureExpList.isEmpty()) return false;
    featureExpList.forEach(this::eval); // To mark all sexps as CODE.
    LispFeatureExp exp0 = featureExpList.get(0);
    if (exp0.getSimpleFeatureExp() == null) return false;
    LispSymbolName symbolName = exp0.getSimpleFeatureExp().getSymbolName();
    switch (symbolName.getValue()) {
      case "NOT":
        if (featureExpList.size() != 2) return false;
        return !eval(featureExpList.get(1));
      case "OR":
        return featureExpList.stream().skip(1).anyMatch(this::eval);
      case "AND":
        return featureExpList.stream().skip(1).allMatch(this::eval);
    }
    return false;
  }

  private boolean isPositive(LispReaderFeature readerFeature) {
    return readerFeature.getText().startsWith("#+");
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
