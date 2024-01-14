package org.ax1.lisp.stubs;

import com.intellij.extapi.psi.StubBasedPsiElementBase;
import com.intellij.lang.ASTNode;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.util.NlsSafe;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.IncorrectOperationException;
import org.ax1.lisp.SymbolResolver;
import org.ax1.lisp.analysis.ProjectData;
import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.analysis.symbol.LexicalSymbol;
import org.ax1.lisp.analysis.symbol.Package;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.psi.*;
import org.ax1.lisp.psi.impl.LispStringDesignator;
import org.ax1.lisp.usages.LispStringDesignatorReference;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.HashSet;
import java.util.Set;

import static com.intellij.lang.annotation.HighlightSeverity.INFORMATION;
import static com.intellij.openapi.editor.DefaultLanguageHighlighterColors.*;
import static com.intellij.openapi.editor.DefaultLanguageHighlighterColors.KEYWORD;
import static org.ax1.lisp.analysis.BaseLispElement.Type.*;

public class LispStringDesignatorBase<T extends StubElement> extends StubBasedPsiElementBase<T> implements LispStringDesignator {

  private static final boolean DEBUG_DESCRIPTION = true;

  private static final Set<Type> SYMBOL_TYPES =
      Set.of(CONDITION_DEFINITION, FUNCTION_DEFINITION, PACKAGE_DEFINITION, VARIABLE_DEFINITION,
          LEXICAL_VARIABLE_DEFINITION, LEXICAL_FUNCTION_DEFINITION, CONDITION_USAGE, VARIABLE_USAGE, PACKAGE_USAGE,
          FUNCTION_USAGE, LEXICAL_VARIABLE_USAGE, LEXICAL_FUNCTION_USAGE);

  // Macros or special forms, whose behavior is closer to keywords, like IF, than to a function call.
  private static final Set<String> KEYWORDS =
      Set.of("CASE", "CCASE", "COND", "CTYPECASE", "DECLAIM", "DECLARE", "DEFCONSTANT", "DEFINE-CONDITION",
          "DEFPACKAGE", "DEFPARAMETER", "DEFMACRO", "DEFUN", "DEFSTRUCT", "DEFVAR", "DO", "DOLIST", "DOSYMBOLS",
          "ECASE", "ETYPECASE", "EVAL-WHEN", "FLET", "HANDLER-BIND", "HANDLER-CASE", "IF", "IGNORE", "IN-PACKAGE",
          "INCF", "LABELS", "LAMBDA", "LET", "LET*", "LOOP", "MACROLET", "PROCLAIM", "RESTART-BIND", "RETURN", "SETF",
          "SETQ", "SPECIAL", "THROW", "TYPECASE", "UNLESS", "WHEN");

  private Type type;
  private String errorMessage;
  private String descriptionString;
  private Package packageDefinition;
  private final Set<String> functionDefinitions = new HashSet<>();
  private LexicalSymbol lexicalVariable;
  private LexicalSymbol lexicalFunction;

  public LispStringDesignatorBase(@NotNull T stub, @NotNull IStubElementType<?, ?> nodeType) {
    super(stub, nodeType);
  }

  public LispStringDesignatorBase(@NotNull ASTNode node) {
    super(node);
  }

  public LispStringDesignatorBase(T stub, IElementType nodeType, ASTNode node) {
    super(stub, nodeType, node);
  }

  @Override
  public PsiElement setName(@NlsSafe @NotNull String newName) throws IncorrectOperationException {
    ASTNode node = getNode();
    node.getTreeParent().replaceChild(node, createNewNode(newName));
    return this;
  }

  public ASTNode createNewNode(@NotNull String newName) {
    // TODO: replace this with overloading.
    if (this instanceof LispStringContent) {
      return LispElementFactory.createStringContent(getProject(), newName).getNode();
    }
    return LispElementFactory.createSymbolName(getProject(), newName).getNode();
  }

  @Override
  public Type getType() {
    LispFile containingFile = (LispFile) getContainingFile();
    SyntaxAnalyzer.INSTANCE.analyze(containingFile);
    if (type == null) {
      type = Type.UNKNOWN;
    }
    return type;
  }

  @Override
  public void setType(Type type) {
    this.type = type;
    // TODO: replace this with overloading.
    if (this instanceof LispSymbolName && getParent() instanceof LispSymbol && type != Type.UNKNOWN && type != Type.COMMENT) {
      LispSymbol lispSymbol = (LispSymbol) getParent();
      LispPackagePrefix packagePrefix = lispSymbol.getPackagePrefix();
      if (packagePrefix != null) {
        packagePrefix.setType(PACKAGE_USAGE);
      }
    }
  }

  @Override
  public void setErrorMessage(String errorMessage) {
    this.errorMessage = errorMessage;
  }

  @Override
  public void clear() {
    errorMessage = null;
    lexicalFunction = null;
    lexicalVariable = null;
    functionDefinitions.clear();
    packageDefinition = null;
  }

  @Override
  public void annotate(@NotNull AnnotationHolder holder) {
    if (errorMessage != null) {
      holder.newAnnotation(HighlightSeverity.ERROR, errorMessage)
          .range(this)
          .create();
      return;
    }
    Type type = getType();
    if (type == Type.DATA && !(this instanceof LispStringContent)) {
      holder.newSilentAnnotation(INFORMATION).range(this).textAttributes(CONSTANT).create();
    } else if (isConstantSymbol()) {
      holder.newSilentAnnotation(INFORMATION).range(this).textAttributes(CONSTANT).create();
    } else if (type == Type.KEYWORD) {
      holder.newSilentAnnotation(INFORMATION).range(this).textAttributes(KEYWORD).create();
    } else if (type == Type.UNKNOWN) {
      holder.newSilentAnnotation(INFORMATION).range(this).textAttributes(REASSIGNED_LOCAL_VARIABLE).create();
    } else if (type == FUNCTION_USAGE && KEYWORDS.contains(getLispName())) {
      holder.newSilentAnnotation(INFORMATION).range(this).textAttributes(KEYWORD).create();
    }

    // TODO: "Function '%s' does not exist"
    // TODO: "Variable '%s' is not defined"
    // TODO: unused lexical variables
    // TODO: "Variable '%s' is never used"
    // TODO: "Function '%s' is never called"
    // TODO: argument checker
  }

  @Override
  public String getDescriptionString() {
    String result = "";
    if (DEBUG_DESCRIPTION) {
      result = getDebugDescription();
    }
    return result + getFormattedDescription();
  }

  @Nullable
  private String getFormattedDescription() {
    switch (type) {
      case FUNCTION_USAGE:
      case FUNCTION_DEFINITION:
      case LEXICAL_FUNCTION_DEFINITION:
      case LEXICAL_FUNCTION_USAGE:
        return String.format("Function %s", getLispName());
      case VARIABLE_DEFINITION:
      case VARIABLE_USAGE:
      case LEXICAL_VARIABLE_DEFINITION:
      case LEXICAL_VARIABLE_USAGE:
        return String.format("Variable %s", getLispName());
      case PACKAGE_USAGE:
      case PACKAGE_DEFINITION:
        return String.format("Package %s", getLispName());
      case CONDITION_DEFINITION:
      case CONDITION_USAGE:
        return String.format("Condition %s", getLispName());
      case METHOD_DEFINITION:
        return String.format("Method %s", getLispName());
      case SYMBOL_USAGE:
        return String.format("Symbol %s", getLispName());
      default:
        return null;
    }
  }

  private String getDebugDescription() {
    return String.format("Class: %s<br>" +
            "Type: %s<br>" +
            "Lisp name: %s<br>" +
            "Description: %s<br>" +
            "Package definition: %s<br>" +
            "Function definitions: %s<br>" +
            "Lexical variable: %s<br>" +
            "Lexical function: %s<br>" +
            "Hash: %d<BR>",
        getClass().getName(),
        getType(),
        getLispName(),
        descriptionString,
        packageDefinition,
        functionDefinitions,
        lexicalVariable,
        lexicalFunction,
        hashCode());
  }

  @Override
  public Package getPackageDefinition() {
    return packageDefinition;
  }

  @Override
  public void setPackageDefinition(Package packageDefinition) {
    this.packageDefinition = packageDefinition;
  }

  @Override
  public void addFunctionDefinition(String functionName) {
    if (type == null) setType(FUNCTION_DEFINITION);
    functionDefinitions.add(functionName);
  }

  @Override
  public void setLexicalVariable(LexicalSymbol lexicalVariable) {
    this.lexicalVariable = lexicalVariable;
  }

  @Override
  public void setLexicalFunction(LexicalSymbol lexicalFunction) {
    this.lexicalFunction = lexicalFunction;
  }

  @Override
  public LexicalSymbol getLexicalVariable() {
    return lexicalVariable;
  }

  @Override
  public LexicalSymbol getLexicalFunction() {
    return lexicalFunction;
  }

  @Override
  public String getLispName() {
    // TODO: take escapes into account.
    if (this instanceof LispStringContent) {
      return getText();
    }
    return getText().toUpperCase();
  }

  @Override
  public String getName() {
    return getText();
  }

  @Override
  public PsiReference getReference() {
    ProjectData projectData = ProjectData.getInstance(getProject());
    if (getType() == FUNCTION_USAGE) {
      return getReference(projectData.getFunctionDefinition(getLispName()));
    }
    if (getType() == VARIABLE_USAGE) {
      return getReference(projectData.getVariableDefinition(getLispName()));
    }
    if (getType() == PACKAGE_USAGE) {
      return getReference(projectData.getPackageDefinition(getLispName()));
    }
    if (getType() == LEXICAL_VARIABLE_USAGE) {
      return getReference(lexicalVariable.definition);
    }
    if (getType() == LEXICAL_FUNCTION_USAGE) {
      return getReference(lexicalFunction.definition);
    }
    if (getType() == Type.SYMBOL_USAGE) {
      LispStringDesignator designator = projectData.getFunctionDefinition(getLispName());
      if (designator == null) designator = projectData.getVariableDefinition(getLispName());
      return getReference(designator);
    }
    return null;
  }

  @Nullable
  private LispStringDesignatorReference getReference(LispStringDesignator functionDefinition) {
    if (functionDefinition != null) {
      return new LispStringDesignatorReference(this, functionDefinition);
    } else {
      return null;
    }
  }

  @Override
  public @Nullable PsiElement getNameIdentifier() {
    return isSymbol() ? this : null;
  }

  private boolean isSymbol() {
    return SYMBOL_TYPES.contains(getType());
  }

  private boolean isConstantSymbol() {
    if (!isSymbol()) return false;
    if (! (this instanceof LispSymbolName)) return false;
    Symbol symbol = SymbolResolver.resolve((LispSymbolName) this);
    return symbol.isConstant();
  }
}
