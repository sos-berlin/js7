package js7.subagent.jobs

import cats.effect.IO
import cats.syntax.parallel.*
import js7.base.io.process.{Stderr, Stdout, StdoutOrStderr}
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.{=>?, flatten}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.order.OrderOutcome
import js7.data.value.{StringValue, Value}
import js7.launcher.OrderProcess
import js7.launcher.internal.InternalJob
import js7.subagent.jobs.TestJob.*
import scala.util.chaining.*

final class TestJob(outcome: OrderOutcome.Completed)
extends InternalJob:
  // We need an empty constructor for reflection
  def this() = this(OrderOutcome.succeeded)

  def toOrderProcess(step: Step): OrderProcess =
    def toStringOrInt: Value =>? Checked[Either[String, Int]] =
      case StringValue(s) => Right(Left(s))
      case v => v.asInt.map(Right(_))

    def writeSomething(arg: Option[Either[String, Int]], outerr: StdoutOrStderr): Option[IO[Unit]] =
      arg.map:
        case Left(string) => step.write(outerr, string).void
        case Right(n) =>
          (1 to n / charBlockSize).foldMap: _ =>
            step.write(outerr, charBlock).void
          .productR:
            step.write(outerr, charBlock.take(n % charBlockSize)).void

    OrderProcess.cancelable:
      for
        stdoutEither <- step.maybeArg("stdout")(toStringOrInt)
        stderrEither <- step.maybeArg("stderr")(toStringOrInt)
        sleep <- step.maybeArg("sleep")(_.asDuration)
        failString <- step.maybeArg("fail")(_.asString)
      yield
        // Try to return fast IO.unit when nothing is done
        val ios = flatten(
          writeSomething(stdoutEither, Stdout),
          writeSomething(stderrEither, Stderr),
          sleep.map(IO.sleep))
        IO.whenA(ios.nonEmpty):
          ios.parSequenceVoid
        .as:
          failString match
            case Some(failString) => OrderOutcome.Failed(Some(failString))
            case None => OrderOutcome.succeeded


object TestJob extends InternalJob.Companion[TestJob]:
  private val logger = Logger[this.type]

  private val charBlockSize = 64 * 1024
  private lazy val charBlock: String =
    val chars: String = ('\u0021' to '\u007e').mkString
    assertThat(chars.length == 94)
    (chars * (charBlockSize / chars.length + 1)).take(charBlockSize)
      .tap: string =>
        assertThat(string.length == charBlockSize)
