package org.ax1.lisp.formatting;

import com.intellij.formatting.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.TokenType;
import com.intellij.psi.formatter.common.AbstractBlock;
import com.intellij.psi.tree.IElementType;
import org.ax1.lisp.psi.LispFile;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispPrefixedSexp;
import org.ax1.lisp.psi.LispSexp;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.ax1.lisp.psi.LispTypes.*;

public class LispBlock extends AbstractBlock {

  /**
   * The default alignment of arguments is the one of function calls, based on argument-1:
   *   (my-function my-argument1
   *                my-argument2)
   * Some macros, specials, and even sometimes functions, have a special first argument, and alignment should only
   * start at argument 2 or 3. For example, WHEN and IF alignment should be based on argument-2:
   *   (if (some-condition)
   *     (do-some-code)
   *     (do-some-other-code))
   *   (if (some-condition) (do-some-code)
   *                        (do-some-other-code)
   * DEFUN should be based on argument-3:
   *   (defun my-function (some params)
   *     (do-some-code)
   *     (do-some-more-code))
   * The variables of a LET should all be aligned from the first one, so that would an argument-0 (the LET itself
   * is an argument-2):
   *   (let ((var1 (init1))
   *         (var2 (init2)))
   *     (do-some-code))
   */
  private static final Set<String> ALIGNMENT2 =
      Set.of("CASE", "DEFPACKAGE", "DO-ALL-SYMBOLS", "DO-EXTERNAL-SYMBOLS", "DO-SYMBOLS", "DOLIST",
          "DOTIMES", "EVAL-WHEN", "ERROR", "HANDLER-CASE", "IF", "LAMBDA", "LET", "LET*", "UNLESS", "WHEN",
          "WITH-OPEN-FILE", "WITH-INPUT-FROM-STRING", "WITH-OUTPUT-TO-STRING");
  private static final Set<String> ALIGNMENT3 =
      Set.of("DEFINE-CONDITION", "DEFUN", "DESTRUCTURING-BIND", "MULTIPLE-VALUE-BIND");
  private static final Set<String> FIRST_ARG_IS_ALIGNMENT0 = Set.of("LET", "LET*", "LABELS", "FLET");

  private int alignmentMode = 1;
  private final Alignment childAlignment = Alignment.createAlignment(false);

  public LispBlock(@NotNull ASTNode node, Alignment alignment) {
    super(node, Wrap.createWrap(WrapType.NONE, false), alignment);
  }

  @Override
  protected List<Block> buildChildren() {
    // We want to cover the following cases:
    // - myNode is a LISP_FILE, its children are PREFIXED_SEXP -> scan it
    // - myNode is a PREFIXED_SEXP, it can have multiple children -> scan it
    // - myNode is a SEXP, and its child is a LIST -> scan that list
    // - myNode is a SEXP, and its child is not a LIST -> empty list
    boolean firstArgAlignment0 = false;
    List<Block> children = new ArrayList<>();
    ASTNode node = myNode;
    if (myNode.getElementType() == SEXP) {
      node = myNode.getFirstChildNode();
      if (node.getElementType() != LIST) {
        return EMPTY;
      }
    }
    if (myNode.getElementType() == LPAREN) {
      return EMPTY;
    }
    for (ASTNode child = node.getFirstChildNode(); child != null ; child = child.getTreeNext()) {
      if (child.getElementType() == TokenType.WHITE_SPACE) continue;
      if (children.size() == 1) {
        // The name is the second child (the first child is an open paren).
        String name = child.getText().toUpperCase();
        if (ALIGNMENT2.contains(name)) alignmentMode = 2;
        if (ALIGNMENT3.contains(name)) alignmentMode = 3;
        if (FIRST_ARG_IS_ALIGNMENT0.contains(name)) firstArgAlignment0 = true;
      }
      LispBlock block = new LispBlock(child, children.size() >= alignmentMode + 1 ? childAlignment : null);
      // In case of firstArgAlignment0, we set isAlignment0 on the child, but that child is just the intermediate SEXP
      // node, we have to propagate it one more time to reach the list.
      if ((children.size() == 2 && firstArgAlignment0) || (alignmentMode == 0 && node.getElementType() == SEXP)) {
        block.alignmentMode = 0;
      }
      children.add(block);
    }
    return children;
  }

  private boolean isParen() {
    IElementType elementType = getNode().getElementType();
    return elementType == LPAREN || elementType == RPAREN;
  }

  @Override
  public @Nullable Spacing getSpacing(@Nullable Block child1, @NotNull Block child2) {
    // One empty line after each top level sexp.
    if (getNode().getPsi() instanceof LispFile) {
      if (child1 != null) {
        PsiElement element = ((LispBlock) child1).getNode().getPsi();
        if (element instanceof LispSexp) return Spacing.createSpacing(0, 0, 2, true, 2);
      }
    }
    // No spaces before or after parens.
    if ((child1 != null && ((LispBlock)child1).isParen()) || ((LispBlock)child2).isParen()) {
      return Spacing.createSpacing(0, 0, 0, true, 0);
    }
    // Exactly one space between two sexp.
    if (child1 != null) {
      PsiElement element1 = ((LispBlock) child1).getNode().getPsi();
      PsiElement element2 = ((LispBlock) child2).getNode().getPsi();
      if (element1 instanceof LispSexp && element2 instanceof LispSexp) {
        return Spacing.createSpacing(1, 1, 0, true, 1);
      }
    }
    return null;
  }

  @Override
  public Indent getIndent() {
    // - If we are not a PrefixedSexp, we have no indent, otherwise we would end up with multiple indent levels instead
    // of one between a list and its elements.
    // - If we are the first element of our parents, all the way to root, we are a toplevel, and we have an absolute
    // none indent.
    // - Otherwise, we have a normal indent.
    PsiElement element = getNode().getPsi();
    if (!(element instanceof LispPrefixedSexp)) {
      return Indent.getNoneIndent();
    }
    PsiElement parent = element.getParent();
    while (true) {
      if (parent instanceof LispFile) {
        return Indent.getAbsoluteNoneIndent();
      }
      PsiElement firstChild = parent.getFirstChild();
      if (element != firstChild) {
        return Indent.getNormalIndent();
      }
      element = parent;
      parent = parent.getParent();
    }
  }

  @Override
  protected Indent getChildIndent() {
    // Must be the same as getIndent(), but from the parent perspective.
    // Top level blocks are absolutely not indented.
    if (getNode().getPsi() instanceof LispFile) {
      return Indent.getAbsoluteNoneIndent();
    }
    // All sexps are indented relative to their parent list.
    if (getNode().getPsi() instanceof LispList) {
      return Indent.getNormalIndent();
    }
    // Everything else is not indented.
    return Indent.getNoneIndent();
  }

  @Override
  public boolean isLeaf() {
    return false;
  }

  @Override
  public @NotNull ChildAttributes getChildAttributes(int newChildIndex) {
    return new ChildAttributes(getChildIndent(), newChildIndex > alignmentMode ? childAlignment : null);
  }

  @Override
  public @Nullable String getDebugName() {
    return "Lisp(" + myNode.getElementType() + ")";
  }
}
