package js7.data.workflow

import fastparse.NoWhitespace._
import fastparse._
import js7.base.io.process.ReturnCode
import js7.base.problem.Checked
import js7.base.time.ScalaTime._
import js7.base.utils.Collections.implicits.RichIterable
import js7.data.agent.AgentPath
import js7.data.job.{CommandLineExecutable, CommandLineParser, InternalExecutable, JobResourcePath, PathExecutable, ReturnCodeMeaning, ShellScriptExecutable}
import js7.data.lock.LockPath
import js7.data.parser.BasicParsers._
import js7.data.parser.Parsers.checkedParse
import js7.data.source.SourcePos
import js7.data.value.expression.Expression.{BooleanConstant, ObjectExpression}
import js7.data.value.expression.ExpressionParser.{booleanConstant, constantExpression, expression}
import js7.data.value.expression.{Expression, Scope}
import js7.data.value.{NamedValues, ObjectValue}
import js7.data.workflow.Instruction.Labeled
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, ExplicitEnd, Finish, Fork, Goto, If, IfFailedGoto, ImplicitEnd, LockInstruction, Prompt, Retry, TryInstruction, End => EndInstr, Fail => FailInstr}
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
object WorkflowParser
{
  // TODO Add OrderPreparation, also in WorkflowPrinter

  def parse(string: String): Checked[Workflow] =
    parse(WorkflowPath.NoId, string)

  def parse(id: WorkflowId, string: String): Checked[Workflow] =
    checkedParse(string, parser.whole(_))
      .map(_.copy(id = id, source = Some(string)))

  private object parser
  {
    private def label[_: P] = identifier map Label.apply

    private def hardEnd[_: P]: P[Int] =
      Index ~ w ~/ instructionTerminator

    private def instructionTerminator[_: P] = P((";" ~ w) | &("}") | &(keyword("else")) | End)
    //Scala-like: val instructionTerminator = P(h ~ (newline | (";" ~ w) | &("}") | End))

    private def workflowDefinition[_: P] = P[Workflow](
      keyword("define") ~ w ~/ keyword("workflow") ~ w ~/ curlyWorkflow.flatMap(o => checkedToP(o.completelyChecked)))

    private def curlyWorkflow[_: P] = P[Workflow](
      ("{" ~ w ~/ (labeledInstruction | jobDefinition).rep ~ w ~ Index ~ "}" ~ Index)
        .flatMap { case (items, start, end) =>
          val jobs = items.collect { case (name: WorkflowJob.Name, job: WorkflowJob) => name -> job }
          jobs.duplicateKeys(_._1) match {
            case Some(dups) =>
              Fail.opaque(s"unique job definitions (duplicates: ${dups.keys.mkString(", ")})")
            case None =>
              val instructions = items.collect { case o: Instruction.Labeled => o } .toVector
              checkedToP(
                Workflow.checkedSub(
                  WorkflowPath.NoId,
                  if (Workflow.isCorrectlyEnded(instructions)) instructions
                  else instructions :+ (() @: ImplicitEnd(Some(SourcePos(start, end)))),
                  jobs.toMap))
          }
        })

    private def curlyWorkflowOrInstruction[_: P] = P[Workflow](
      curlyWorkflow | instruction.map(o => Workflow.anonymous(Vector(o))))

    private def labelDef[_: P] = P[Label](
      label ~ h ~ ":" ~/ w)

    private def returnCode[_: P] = P[ReturnCode](int map ReturnCode.apply)

    private def successReturnCodes[_: P] = P[ReturnCodeMeaning.Success](
      bracketCommaSequence(returnCode)
        map(returnCodes => ReturnCodeMeaning.Success(returnCodes.toSet)))

    private def failureReturnCodes[_: P] = P[ReturnCodeMeaning.Failure](
      bracketCommaSequence(returnCode)
        map(returnCodes => ReturnCodeMeaning.Failure(returnCodes.toSet)))

    private def endInstruction[_: P] = P[EndInstr](
      Index ~ keyword("end") ~ hardEnd
        map { case (start, end) => ExplicitEnd(sourcePos(start, end)) })

    private def objectExpression[_: P]: P[ObjectExpression] = P(
      expressionMap
        .map(ObjectExpression(_)))

    private def expressionMap[_: P]: P[Map[String, Expression]] = P(
      curly(nonEmptyCommaSequence(quotedString ~ w ~ ":" ~ w ~/ expression))
       .map(_.toMap))

    private def anonymousWorkflowJob[_: P] = P[WorkflowJob](
      for {
        kv <- keyValues(
          keyValue("env", objectExpression) |
          keyValue("v1Compatible", booleanConstant) |
          keyValue("executable", quotedString) |
          keyValue("command", quotedString) |
          keyValue("script", constantExpression) |
          keyValue("internalJobClass", constantExpression) |
          keyValue("agent", agentPath) |
          keyValue("defaultArguments", objectExpression) |
          keyValue("arguments", objectExpression) |
          keyValue("jobArguments", objectExpression) |
          keyValue("jobResourcePaths", inParentheses(commaSequence(quotedString.map(JobResourcePath(_))))) |
          keyValue("successReturnCodes", successReturnCodes) |
          keyValue("failureReturnCodes", failureReturnCodes) |
          keyValue("parallelism", int) |
          keyValue("sigkillDelay", int))
        agentPath <- kv[AgentPath]("agent")
        defaultArguments <- kv[ObjectExpression]("defaultArguments", ObjectExpression.empty)
        arguments <- kv[ObjectExpression]("arguments", ObjectExpression.empty)
        jobArguments <- kv[ObjectExpression]("jobArguments", ObjectExpression.empty)
        jobResourcePaths <- kv[Seq[JobResourcePath]]("jobResourcePaths", Nil)
        env <- kv[ObjectExpression]("env", ObjectExpression.empty)
        v1Compatible <- kv.noneOrOneOf[BooleanConstant]("v1Compatible").map(_.fold(false)(_._2.booleanValue))
        returnCodeMeaning <- kv.oneOfOr(Set("successReturnCodes", "failureReturnCodes"), ReturnCodeMeaning.Default)
        executable <- kv.oneOf[Any]("executable", "command", "script", "internalJobClass").flatMap {
          case ("executable", path: String) =>
            Pass(PathExecutable(path, env.nameToExpr, returnCodeMeaning = returnCodeMeaning, v1Compatible = v1Compatible))
          case ("command", command: String) =>
            if (v1Compatible) Fail.opaque(s"v1Compatible=true is inappropriate for a command")
            else checkedToP(CommandLineParser.parse(command)
              .map(CommandLineExecutable(_, env.nameToExpr, returnCodeMeaning = returnCodeMeaning)))
          case ("script", script: Expression) =>
            checkedToP(script.eval(Scope.empty).flatMap(_.asStringValue)
              .map(v => ShellScriptExecutable(v.string, env.nameToExpr, returnCodeMeaning = returnCodeMeaning, v1Compatible = v1Compatible)))
          case ("internalJobClass", className: Expression) =>
            checkedToP(className.eval(Scope.empty).flatMap(_.asStringValue)
              .map(v => InternalExecutable(v.string, jobArguments.nameToExpr, arguments.nameToExpr)))
          case _ => Fail.opaque("Invalid executable")  // Does not happen
        }
        parallelism <- kv[Int]("parallelism", WorkflowJob.DefaultParallelism)
        sigkillDelay <- kv.get[Int]("sigkillDelay").map(_.map(_.s))
      } yield
        WorkflowJob(agentPath, executable, defaultArguments.nameToExpr, jobResourcePaths,
          parallelism = parallelism,
          sigkillDelay = sigkillDelay))

    private def executeInstruction[_: P] = P[Execute.Anonymous](
      (Index ~ keyword("execute") ~ w ~ anonymousWorkflowJob ~ hardEnd)
        .map { case (start, job, end) => Execute.Anonymous(job, sourcePos = sourcePos(start, end)) })

    private def jobInstruction[_: P] = P[Execute](
      (Index ~ keyword("job") ~ w ~ identifier ~ (w ~ comma ~ keyValue("defaultArguments", objectExpression)).? ~ hardEnd)
        .flatMap {
          case (start, name, None, end) =>
            valid(Execute.Named(WorkflowJob.Name(name), sourcePos = sourcePos(start, end)))
          case (start, name, Some(("defaultArguments", objectExpression)), end) =>
            valid(Execute.Named(WorkflowJob.Name(name), defaultArguments = objectExpression.nameToExpr,
              sourcePos(start, end)))
          case (_, _, Some((keyword, _)), _) =>
            invalid(s"Unexpected keyword: $keyword")
        })

    private def failInstruction[_: P] = P[FailInstr](
      (Index ~ keyword("fail") ~
        inParentheses(keyValues(
          keyValueConvert("namedValues", objectExpression)(o =>
            o.eval(Scope.empty).map(_.asInstanceOf[ObjectValue].nameToValue)) |
          keyValue("message", expression) |
          keyValue("uncatchable", booleanConstant))).? ~
        hardEnd)
        .flatMap { case (start, maybeKeyToValue, end) =>
          val keyToValue = maybeKeyToValue getOrElse KeyToValue.empty
          for {
            namedValues <- keyToValue.get[NamedValues]("namedValues")
            errorMessage <- keyToValue.get[Expression]("message")
            uncatchable <- keyToValue.get[BooleanConstant]("uncatchable").map(_.fold(false)(_.booleanValue))
          } yield FailInstr(errorMessage, namedValues getOrElse Map.empty, uncatchable = uncatchable, sourcePos(start, end))
        })

    private def promptInstruction[_: P] = P[Prompt](
      (Index ~ keyword("prompt") ~ w ~/ expression ~ hardEnd)
        .map { case (start, expression, end) =>
          Prompt(expression, sourcePos(start, end))
        })

    private def finishInstruction[_: P] = P[Finish](
      (Index ~ keyword("finish") ~ hardEnd)
        .map { case (start, end) => Finish(sourcePos(start, end)) })

    private def forkInstruction[_: P] = P[Fork]{
      def branchId = P(quotedString.map(o => Fork.Branch.Id(o)))
      def forkBranch = P[Fork.Branch](
        (branchId ~ w ~ ":" ~ w ~ curlyWorkflowOrInstruction)
          map Fork.Branch.fromPair)
      (Index ~ keyword("fork") ~ Index ~ w ~ curly(w ~ forkBranch ~ (comma ~ forkBranch).rep) ~ w ~ instructionTerminator.?)
        .flatMap { case (start, end, (branch, more)) =>
          checkedToP(Fork.checked(Vector(branch) ++ more, agentPath = None, sourcePos(start, end)))
        }
    }

    private def ifInstruction[_: P] = P[If](
      (Index ~ keyword("if") ~ w ~/ inParentheses(expression) ~ Index ~
        w ~/ curlyWorkflowOrInstruction ~/
        (w ~ "else" ~ w ~/ curlyWorkflowOrInstruction).? ~
        w ~/ instructionTerminator.?
      ) map { case (start, expr, end, then_, else_) =>
        If(expr, then_, else_, sourcePos(start, end))
      })

    private def retryInstruction[_: P] = P[Retry](
      (Index ~ keyword("retry") ~ hardEnd)
        .map { case (start, end) => Retry(sourcePos(start, end)) })

    private def tryInstruction[_: P] = P[TryInstruction](
      (Index ~ keyword("try") ~
        (w ~ inParentheses(keyValues(
          keyValue("retryDelays", bracketCommaSequence(int)) |
          keyValue("maxTries", int)))
        ).?.map(_ getOrElse KeyToValue.empty) ~
        Index ~
        w ~/ curlyWorkflowOrInstruction ~ w ~/
        keyword("catch") ~ w ~/
        curlyWorkflowOrInstruction ~
        w ~/ instructionTerminator.?
      ) .flatMap { case (start, keyToValue, end, try_, catch_) =>
          for {
            delays <- keyToValue.get[Seq[Int]]("retryDelays")
            maxTries <- keyToValue.get[Int]("maxTries")
            try_ <- checkedToP(TryInstruction.checked(try_, catch_,
              delays.map(_.toVector.map(FiniteDuration(_, SECONDS))),
              maxTries = maxTries,
              sourcePos(start, end)))
          } yield try_
        })

    private def ifFailedGotoInstruction[_: P] = P[IfFailedGoto](
      (Index ~ keyword("ifFailedGoto") ~ w ~ label ~ hardEnd)
        .map { case (start, n, end) => IfFailedGoto(n, sourcePos(start, end)) })

    private def gotoInstruction[_: P] = P[Goto](
      Index ~ keyword("goto") ~ w ~ label ~ hardEnd
        map { case (start, n, end) => Goto(n, sourcePos(start, end)) })

    private def lockInstruction[_: P] = P[LockInstruction](
      (Index ~ keyword("lock") ~ w ~/
        inParentheses(keyValues(
          keyValue("lock", quotedLockPath) |
          keyValue("count", int)
        )) ~/
        Index ~/
        w ~/ curlyWorkflowOrInstruction
      ).flatMap { case (start, keyToValue, end, subworkflow) =>
        for {
          lockPath <- keyToValue[LockPath]("lock")
          count <- keyToValue.get[Int]("count")
          lock <- checkedToP(LockInstruction.checked(lockPath, count, subworkflow, sourcePos(start, end)))
        } yield lock
      }  ~~/ instructionTerminator.?)

    private def instruction[_: P]: P[Instruction] =
      P(endInstruction |
        executeInstruction |
        failInstruction |
        finishInstruction |
        forkInstruction |
        gotoInstruction |
        ifInstruction |
        ifFailedGotoInstruction |
        jobInstruction |
        promptInstruction |
        retryInstruction |
        tryInstruction |
        lockInstruction)

    private def labeledInstruction[_: P] = P[Labeled](
      (labelDef.? ~ instruction)
        map { case (maybeLabel, instruction_) => Labeled(maybeLabel, instruction_)})

    private def jobDefinition[_: P] = P[(WorkflowJob.Name, WorkflowJob)](
      keyword("define") ~ w ~/ keyword("job") ~ w ~/
        identifier.map(WorkflowJob.Name.apply) ~ w ~/
        curly(executeInstruction ~ w ~ instructionTerminator).map(_.job) ~/
        w)

    def whole[_: P] = P[Workflow](w ~/ workflowDefinition ~ w ~/ End)
  }

  private def sourcePos(start: Int, end: Int) = Some(SourcePos(start, end))
}
