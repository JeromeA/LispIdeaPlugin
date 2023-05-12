package org.ax1.lisp.parsing;

import com.intellij.lang.ASTNode;
import com.intellij.lang.ParserDefinition;
import com.intellij.lang.PsiParser;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.project.Project;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IFileElementType;
import com.intellij.psi.tree.TokenSet;
import org.ax1.lisp.LispLanguage;
import org.ax1.lisp.psi.LispFile;
import org.ax1.lisp.psi.LispTypes;
import org.ax1.lisp.stubs.LispFileElementType;
import org.jetbrains.annotations.NotNull;

public class LispParserDefinition implements ParserDefinition {
  public static final TokenSet WHITE_SPACES = TokenSet.create(TokenType.WHITE_SPACE);
  public static final TokenSet COMMENTS = TokenSet.create(LispTypes.COMMENT);

  @NotNull
  @Override
  public Lexer createLexer(Project project) {
    return new LispLexerAdapter();
  }

  @NotNull
  @Override
  public TokenSet getWhitespaceTokens() {
    return WHITE_SPACES;
  }

  @NotNull
  @Override
  public TokenSet getCommentTokens() {
    return COMMENTS;
  }

  @NotNull
  @Override
  public TokenSet getStringLiteralElements() {
    return TokenSet.EMPTY;
  }

  @NotNull
  @Override
  public PsiParser createParser(final Project project) {
    return new LispParser();
  }

  @NotNull
  @Override
  public IFileElementType getFileNodeType() {
    return LispFileElementType.INSTANCE;
  }

  @NotNull
  @Override
  public PsiFile createFile(@NotNull FileViewProvider viewProvider) {
    return new LispFile(viewProvider);
  }

  @NotNull
  @Override
  public SpaceRequirements spaceExistenceTypeBetweenTokens(ASTNode left, ASTNode right) {
    return SpaceRequirements.MAY;
  }

  @NotNull
  @Override
  public PsiElement createElement(ASTNode node) {
    return LispTypes.Factory.createElement(node);
  }

}
