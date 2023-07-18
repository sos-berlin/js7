package js7.data.workflow

import cats.parse.Parser.{char, end, failWith, index, peek, pure, string}
import cats.parse.{Parser, Parser0}
import js7.base.io.process.ReturnCode
import js7.base.parser.BasicParsers.*
import js7.base.parser.Parsers.checkedParse
import js7.base.parser.Parsers.syntax.*
import js7.base.problem.Checked
import js7.base.time.ScalaTime.*
import js7.base.utils.Collections.implicits.*
import js7.base.utils.RangeSet
import js7.data.agent.AgentPath
import js7.data.job.{CommandLineExecutable, CommandLineParser, InternalExecutable, JobResourcePath, PathExecutable, ReturnCodeMeaning, ShellScriptExecutable}
import js7.data.lock.LockPath
import js7.data.parser.Js7Parsers.path
import js7.data.source.SourcePos
import js7.data.value.expression.Expression.{BooleanConstant, ObjectExpr}
import js7.data.value.expression.ExpressionParser.{booleanConstant, constantExpression, expression}
import js7.data.value.expression.{Expression, Scope}
import js7.data.value.{NamedValues, ObjectValue}
import js7.data.workflow.Instruction.Labeled
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, ExplicitEnd, Finish, Fork, If, ImplicitEnd, LockInstruction, Prompt, Retry, TryInstruction, End as EndInstr, Fail as FailInstr}
import js7.data.workflow.position.Label
import scala.concurrent.duration.*

object WorkflowParser
{
  // TODO Add OrderPreparation, also in WorkflowPrinter

  def parse(string: String): Checked[Workflow] =
    parse(WorkflowPath.NoId, string)

  def parse(id: WorkflowId, string: String): Checked[Workflow] =
    checkedParse(string, parser.whole)
      .map(_.copy(id = id, source = Some(string)))

  private object parser
  {
    private val label: Parser[Label] =
      identifier map Label.apply

    private val instructionTerminator: Parser0[Unit] =
      (w ~
        ((char(';') ~ w).void |
          peek(char('}') | keyword("else") | keyword("catch")) |
          end
        ).backtrack.orElse(failWith("Expected end of instruction"))
      ).void

    //Scala-like: val instructionTerminator = P(h ~ (newline | (";" ~ w) | peek("}") | End))

    private val hardEnd: Parser0[Int] =
      index <* instructionTerminator

    private val labelDef: Parser[Label] =
      (label ~ h ~ char(':') ~ w)
        .map { case (((label, ()), ()), ()) => label }

    private val returnCode: Parser[ReturnCode] =
      int map ReturnCode.apply

    private val successReturnCodes: Parser[ReturnCodeMeaning.Success] =
      bracketCommaSequence(returnCode)
        .map(returnCodes => ReturnCodeMeaning.Success(RangeSet(returnCodes*)))

    private val failureReturnCodes: Parser[ReturnCodeMeaning.Failure] =
      bracketCommaSequence(returnCode)
        .map(returnCodes => ReturnCodeMeaning.Failure(RangeSet(returnCodes*)))

    private val endInstruction: Parser[EndInstr] =
      (index.with1 ~ keyword("end") ~ hardEnd)
        .map { case ((start, ()), end) =>
          ExplicitEnd(sourcePos(start, end))
        }

    private val expressionMap: Parser[Map[String, Expression]] =
      curly(
        nonEmptyCommaSequence((quotedString ~~<* char(':') ~~ expression))
          .map(_.toList.toMap))

    private val objectExpression: Parser[ObjectExpr] =
      expressionMap
        .map(ObjectExpr.apply)

    private val anonymousWorkflowJob: Parser0[WorkflowJob] = for {
      kv <- keyValues[Any](
        keyValue("env", objectExpression) |
        keyValue("v1Compatible", booleanConstant) |
        keyValue("executable", quotedString) |
        keyValue("command", quotedString) |
        keyValue("script", constantExpression) |
        keyValue("internalJobClass", constantExpression) |
        keyValue("agent", path(AgentPath)) |
        keyValue("defaultArguments", objectExpression) |
        keyValue("arguments", objectExpression) |
        keyValue("jobArguments", objectExpression) |
        keyValue("jobResourcePaths", inParentheses(commaSequence(quotedString.map(JobResourcePath.apply)))) |
        keyValue("successReturnCodes", successReturnCodes) |
        keyValue("failureReturnCodes", failureReturnCodes) |
        keyValue("parallelism", int) |
        keyValue("sigkillDelay", int))
      agentPath <- kv[AgentPath]("agent")
      defaultArguments <- kv[ObjectExpr]("defaultArguments", ObjectExpr.empty)
      arguments <- kv[ObjectExpr]("arguments", ObjectExpr.empty)
      jobArguments <- kv[ObjectExpr]("jobArguments", ObjectExpr.empty)
      jobResourcePaths <- kv[Seq[JobResourcePath]]("jobResourcePaths", Nil)
      env <- kv[ObjectExpr]("env", ObjectExpr.empty)
      v1Compatible <- kv.noneOrOneOf[BooleanConstant]("v1Compatible").map(_.fold(false)(_._2.booleanValue))
      returnCodeMeaning <- kv.oneOfOr(Set("successReturnCodes", "failureReturnCodes"), ReturnCodeMeaning.Default)
      executable <- kv.oneOf[Any]("executable", "command", "script", "internalJobClass").flatMap {
        case ("executable", path: String) =>
          pure(PathExecutable(path, env.nameToExpr, returnCodeMeaning = returnCodeMeaning, v1Compatible = v1Compatible))
        case ("command", command: String) =>
          if (v1Compatible) failWith(s"v1Compatible=true is inappropriate for a command")
          else checkedToParser(CommandLineParser.parse(command)
            .map(CommandLineExecutable(_, env.nameToExpr, returnCodeMeaning = returnCodeMeaning)))
        case ("script", script: Expression) =>
          checkedToParser(script.evalAsString(Scope.empty)
            .map(string => ShellScriptExecutable(string, env.nameToExpr, returnCodeMeaning = returnCodeMeaning, v1Compatible = v1Compatible)))
        case ("internalJobClass", className: Expression) =>
          checkedToParser(className.evalAsString(Scope.empty)
            .map(string => InternalExecutable(className = string,
              jobArguments = jobArguments.nameToExpr,
              arguments = arguments.nameToExpr)))
        case _ => failWith("Invalid executable")  // Does not happen
      }
      parallelism <- kv[Int]("parallelism", WorkflowJob.DefaultParallelism)
      sigkillDelay <- kv.get[Int]("sigkillDelay").map(_.map(_.s))
    } yield
      WorkflowJob(agentPath, executable, defaultArguments.nameToExpr,
        subagentSelectionId = None/*TODO*/,
        jobResourcePaths,
        parallelism = parallelism,
        sigkillDelay = sigkillDelay)

    private val executeInstruction: Parser[Execute.Anonymous] =
      (index ~<* keyword("execute") ~~ anonymousWorkflowJob ~ hardEnd)
        .map { case ((start, job), end) =>
          Execute.Anonymous(job, sourcePos = sourcePos(start, end))
        }

    private val jobInstruction: Parser[Execute] =
      (index
        ~<* keyword("job")
        ~~ identifier
        ~~ (string(",") *> w *> keyValue("defaultArguments", objectExpression)).?
        ~ hardEnd
      ).flatMap {
        case (((start, name), None), end) =>
          pure(Execute.Named(
            WorkflowJob.Name(name),
            sourcePos = sourcePos(start, end)))

        case (((start, name), Some((_, objectExpression))), end) =>
          pure(Execute.Named(
            WorkflowJob.Name(name),
            defaultArguments = objectExpression.nameToExpr,
            sourcePos(start, end)))
      }

    private val failInstruction: Parser[FailInstr] =
      (index.with1
        ~ keyword("fail")
        ~ (w *> inParentheses(keyValues(
        keyValueConvert("namedValues", objectExpression)(o =>
          o.evalAs(ObjectValue, Scope.empty).map(_.nameToValue)) |
        keyValue("message", expression) |
        keyValue("uncatchable", booleanConstant)))).backtrack.?
        ~ hardEnd)
      .flatMap { case (((start, ()), maybeKeyToValue), end) =>
        val keyToValue = maybeKeyToValue getOrElse KeyToValue.empty
        for {
          namedValues <- keyToValue.get[NamedValues]("namedValues")
          errorMessage <- keyToValue.get[Expression]("message")
          uncatchable <- keyToValue.get[BooleanConstant]("uncatchable").map(_.fold(false)(_.booleanValue))
        } yield FailInstr(errorMessage, namedValues getOrElse Map.empty, uncatchable = uncatchable, sourcePos(start, end))
      }

    private val promptInstruction: Parser[Prompt] =
      ((index.with1 <* keyword("prompt")) ~~ expression ~ hardEnd)
        .map { case ((start, expression), end) =>
          Prompt(expression, sourcePos(start, end))
        }

    private val finishInstruction: Parser[Finish] =
      (index.with1 ~ keyword("finish") ~ hardEnd)
        .map { case ((start, ()), end) =>
          Finish(sourcePos = sourcePos(start, end))
        }

    private val forkInstruction: Parser[Fork] = {
      def forkBranch: Parser[Fork.Branch] =
        (quotedString ~~<* char(':') ~~ curlyWorkflowOrInstruction)
          .map { case (id, workflow) =>
            Fork.Branch(id, workflow)
          }
      ((index ~<* keyword("fork"))
        ~ (w *> inParentheses(keyValues(keyValue("joinIfFailed", booleanConstant))))
        .backtrack.?
        ~ index
        ~~ curly(commaSequence(forkBranch))
        ~~<* instructionTerminator.?
      ) .flatMap { case (((start, maybeKeyToValue), end), branches) =>
          val keyToValue = maybeKeyToValue getOrElse KeyToValue.empty
          for {
            joinIfFailed <- keyToValue("joinIfFailed", BooleanConstant(false))
            fork <- checkedToParser(Fork.checked(
              branches,
              agentPath = None,
              joinIfFailed = joinIfFailed.booleanValue,
              sourcePos = sourcePos(start, end)))
          } yield fork
        }
    }

    private val ifInstruction: Parser[If] =
      (index.with1
        ~ (keyword("if") ~ w *> (inParentheses(expression) ~ index))
        ~~ curlyWorkflowOrInstruction
        ~~ (string("else") *> w *> curlyWorkflowOrInstruction).?
        ~~<* instructionTerminator.?
      ).map { case (((start, (expr, end)), then_), else_) =>
        If(expr, then_, else_, sourcePos(start, end))
      }

    private val retryInstruction: Parser[Retry] =
      (index.with1 ~ keyword("retry") ~ hardEnd)
        .map { case ((start, ()), end) =>
          Retry(sourcePos(start, end))
        }

    private val tryInstruction: Parser[TryInstruction] =
      (index
        ~<* keyword("try")
        ~ (w *> inParentheses(
          keyValues(
            keyValue("retryDelays", bracketCommaSequence(int)) |
            keyValue("maxTries", int))
        )).backtrack.?.map(_ getOrElse KeyToValue.empty)
        ~ index
        ~~ curlyWorkflowOrInstruction
        ~~<* keyword("catch")
        ~~ curlyWorkflowOrInstruction
        ~~<* instructionTerminator.?
      ) .flatMap { case ((((start, keyToValue), end), try_), catch_) =>
          for {
            delays <- keyToValue.get[Seq[Int]]("retryDelays")
            maxTries <- keyToValue.get[Int]("maxTries")
            try_ <- checkedToParser(TryInstruction.checked(try_, catch_,
              delays.map(_.toVector.map(FiniteDuration(_, SECONDS))),
              maxTries = maxTries,
              sourcePos(start, end)))
          } yield try_
        }

    private val lockInstruction: Parser[LockInstruction] =
      (index ~<*
        keyword("lock") ~~
        inParentheses(keyValues(
          keyValue("lock", path(LockPath)) |
          keyValue("count", int)
        )) ~
        index ~~
        curlyWorkflowOrInstruction
      ).flatMap { case (((start, keyToValue), end), subworkflow) =>
        for {
          lockPath <- keyToValue[LockPath]("lock")
          count <- keyToValue.get[Int]("count")
          lock <- checkedToParser(LockInstruction.checked(lockPath, count, subworkflow, sourcePos(start, end)))
        } yield lock
      } <* instructionTerminator.?

    private val instruction: Parser[Instruction] =
      endInstruction |
        executeInstruction |
        failInstruction |
        finishInstruction |
        forkInstruction |
        ifInstruction |
        jobInstruction |
        promptInstruction |
        retryInstruction |
        tryInstruction |
        lockInstruction

    private val labeledInstruction: Parser[Labeled] = (
      (labelDef.backtrack.?.with1 ~ instruction)
        map { case (maybeLabel, instruction_) =>
        Labeled(maybeLabel, instruction_)
      })

    private val jobDefinition: Parser[(WorkflowJob.Name, WorkflowJob)] =
      (keyword("define") ~~ keyword("job") ~ w) *>
        identifier.map(WorkflowJob.Name.apply) ~~
        curly(
          executeInstruction.map(_.job) ~~<* instructionTerminator.?) <* w

    private val workflowDefinition: Parser[Workflow] =
      ((keyword("define") ~~ keyword("workflow") ~ w) *> curlyWorkflow)
        .flatMap(workflow => checkedToParser(workflow.completelyChecked))

    private lazy val curlyWorkflow: Parser[Workflow] =
      Parser.defer(
        ((char('{') ~ w)
          *> (labeledInstruction | jobDefinition).rep0
          ~~ (index.with1 <* char('}'))
          ~ index)
        .flatMap { case ((items, start), end) =>
          val jobs = items.collect { case (name: WorkflowJob.Name, job: WorkflowJob) =>
            name -> job
          }
          jobs.duplicateKeys(_._1) match {
            case Some(dups) =>
              failWith(s"Expected unique job definitions (duplicates: ${dups.keys.mkString(", ")})")
            case None =>
              val instructions = items.collect { case o: Instruction.Labeled => o } .toVector
              checkedToParser(
                Workflow.checkedSub(
                  WorkflowPath.NoId,
                  if (Workflow.isCorrectlyEnded(instructions)) instructions
                  else instructions :+ (() @: ImplicitEnd(Some(SourcePos(start, end)))),
                  jobs.toMap))
          }
        })

    private lazy val curlyWorkflowOrInstruction: Parser[Workflow] =
      Parser.defer(
        curlyWorkflow | instruction.map(o => Workflow.anonymous(Vector(o))))

    val whole: Parser[Workflow] =
      w.with1 *> workflowDefinition <* w <* end
  }

  private def sourcePos(start: Int, end: Int) = Some(SourcePos(start, end))
}
