package org.ax1.lisp.subprocess;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.console.LanguageConsoleBuilder;
import com.intellij.execution.console.LanguageConsoleView;
import com.intellij.execution.console.ProcessBackedConsoleExecuteActionHandler;
import com.intellij.execution.process.BaseOSProcessHandler;
import com.intellij.execution.process.OSProcessHandler;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.runners.AbstractConsoleRunnerWithHistory;
import com.intellij.execution.target.*;
import com.intellij.execution.target.local.LocalTargetEnvironmentRequest;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.components.Service;
import com.intellij.openapi.progress.EmptyProgressIndicator;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.text.StringUtil;
import org.ax1.lisp.LispLanguage;
import org.ax1.lisp.settings.LispSettingsState;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

import static com.intellij.openapi.components.Service.Level.PROJECT;

@Service(PROJECT)
public final class LispConsole extends AbstractConsoleRunnerWithHistory<LanguageConsoleView> {

  public static final String LISP_REPL_TITLE = "Lisp REPL";
  public static final Key<LispConsole> LISP_CONSOLE = Key.create("Lisp console key");

  private final @NotNull Project project;
  private TargetedCommandLine cmdLine;
  private TargetEnvironment environment;

  public static LispConsole getInstance(Project project) {
    return project.getService(LispConsole.class);
  }

  public LispConsole(@NotNull Project project) {
    super(project, LISP_REPL_TITLE, null);
    this.project = project;
    TargetEnvironmentRequest environmentRequest = new LocalTargetEnvironmentRequest();
    environment = environmentRequest.prepareEnvironment(TargetProgressIndicator.EMPTY);
    TargetedCommandLineBuilder commandLineBuilder = new TargetedCommandLineBuilder(environmentRequest);
    String executable = LispSettingsState.getInstance().selectedBinaryPath;
    commandLineBuilder.setExePath(executable);
    cmdLine = commandLineBuilder.build();
  }

  public void start() {
    try {
      initAndRun();
    } catch (ExecutionException e) {
      throw new RuntimeException("Can't run REPL", e);
    }
  }

  public void evaluate(String sexp) {
    ApplicationManager.getApplication().executeOnPooledThread(() -> send(getProcessHandler(), sexp));
  }

  private void send(ProcessHandler processHandler, String sexp) {
    final OutputStream outputStream = processHandler.getProcessInput();
    byte[] bytes = (sexp + "\n").getBytes();
    try {
      outputStream.write(bytes);
      outputStream.flush();
    }
    catch (IOException e) {
      e.printStackTrace();
    }
  }

  @Override
  protected LanguageConsoleView createConsoleView() {
    LanguageConsoleBuilder builder = new LanguageConsoleBuilder();
    return builder.build(getProject(), LispLanguage.INSTANCE);
  }

  @Override
  protected @Nullable Process createProcess() throws ExecutionException {
    return environment.createProcess(cmdLine, new EmptyProgressIndicator());
  }

  @Override
  protected OSProcessHandler createProcessHandler(Process process) {
    try {
      return new OSProcessHandler(process, cmdLine.getCommandPresentation(environment));
    } catch (ExecutionException e) {
      throw new RuntimeException("Could not build process handler", e);
    }
  }

  @Override
  protected @NotNull ProcessBackedConsoleExecuteActionHandler createExecuteActionHandler() {
    return new ProcessBackedConsoleExecuteActionHandler(getProcessHandler(), false);
  }
}
