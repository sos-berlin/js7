package js7.data.order

import io.circe.*
import io.circe.generic.semiauto.deriveCodec
import io.circe.syntax.*
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.io.process.{ProcessSignal, ReturnCode}
import js7.base.problem.{Checked, Problem}
import js7.base.system.OperatingSystem.isWindows
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.typeclasses.IsEmpty.syntax.*
import js7.data.order.OrderOutcome.Disrupted.ProcessLost
import js7.data.subagent.Problems.ProcessLostDueToUnknownReasonProblem
import js7.data.value.{NamedValues, NumberValue}
import org.jetbrains.annotations.TestOnly
import scala.collection.View
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
sealed trait OrderOutcome:
  final def isSucceeded: Boolean =
    isInstanceOf[OrderOutcome.IsSucceeded]

  def show: String

object OrderOutcome:
  val succeeded: Completed = Succeeded.empty
  val succeededRC0: Succeeded = Succeeded.returnCode0
  val failed: Failed = Failed(None, Map.empty)

  def leftToFailed(checked: Checked[OrderOutcome]): OrderOutcome =
    checked match
      case Left(problem) => OrderOutcome.Failed.fromProblem(problem)
      case Right(o) => o

  def leftToDisrupted(checked: Checked[OrderOutcome]): OrderOutcome =
    checked match
      case Left(problem) => OrderOutcome.Disrupted(problem)
      case Right(o) => o

  @TestOnly
  def failedWithSignal(signal: ProcessSignal): Failed =
    Failed(Map("returnCode" -> NumberValue(if isWindows then 0 else 128 + signal.number)))

  /** The job has terminated. */
  sealed trait Completed extends OrderOutcome:
    def namedValues: NamedValues
  object Completed:
    def apply(success: Boolean): Completed =
      apply(success, Map.empty)

    def apply(success: Boolean, namedValues: NamedValues): Completed =
      if success then
        Succeeded(namedValues)
      else
        Failed(namedValues)

    def fromChecked(checked: Checked[OrderOutcome.Completed]): OrderOutcome.Completed =
      checked match
        case Left(problem) => OrderOutcome.Failed.fromProblem(problem)
        case Right(o) => o

    def fromTry(tried: Try[OrderOutcome.Completed]): Completed =
      tried match
        case Failure(t) => Failed.fromThrowable(t)
        case Success(o) => o

    private[OrderOutcome] sealed trait Companion[A <: Completed]:
      protected def make(namedValues: NamedValues): A

      def rc(returnCode: Int): A =
        make(NamedValues.rc(returnCode))

      def rc(returnCode: ReturnCode): A =
        make(NamedValues.rc(returnCode))

      def apply(): A =
        make(Map.empty)

      def apply(namedValues: NamedValues): A =
        make(namedValues)

    implicit val jsonCodec: TypedJsonCodec[Completed] = TypedJsonCodec(
      Subtype[Failed],
      Subtype(Caught),
      Subtype(deriveCodec[Succeeded]))


  sealed trait IsSucceeded extends Completed


  final case class Succeeded(namedValues: NamedValues) extends IsSucceeded:
    def show: String =
      if namedValues.isEmpty then "Succeeded" else s"Succeeded($namedValues)"
    override def toString: String = show

  object Succeeded extends Completed.Companion[Succeeded]:
    val empty = new Succeeded(Map.empty)
    val returnCode0 = new Succeeded(NamedValues.rc(0))

    protected def make(namedValues: NamedValues): Succeeded =
      if namedValues.isEmpty then
        empty
      else if namedValues == returnCode0.namedValues then
        returnCode0
      else
        new Succeeded(namedValues)

    implicit val jsonEncoder: Encoder.AsObject[Succeeded] =
      o => JsonObject.fromIterable(
        o.namedValues.nonEmpty.thenList("namedValues" -> o.namedValues.asJson))

    implicit val jsonDecoder: Decoder[Succeeded] = c =>
      for namedValues <- c.getOrElse[NamedValues]("namedValues")(Map.empty) yield
        make(namedValues)


  type Caught = Caught.type
  case object Caught extends IsSucceeded:
    def namedValues = NamedValues.empty
    def show = "Caught"


  final case class Failed(
    errorMessage: Option[String],
    namedValues: NamedValues,
    uncatchable: Boolean)
  extends Completed, NotSucceeded:
    override def toString: String =
      View(uncatchable ? "uncatchable", errorMessage, namedValues.??)
        .flatten
        .mkString("⚠️ Failed(", ", ", ")")

    def show: String =
      "Failed" +
        ((uncatchable || errorMessage.isDefined) ??
          ("(" + View(uncatchable ? "uncatchable", errorMessage).flatten.mkString(", ") + ")"))

  object Failed extends Completed.Companion[Failed]:
    def apply(
      errorMessage: Option[String] = None,
      namedValues: NamedValues = Map.empty,
      uncatchable: Boolean = false)
    : Failed =
      new Failed(errorMessage, namedValues, uncatchable)

    def fromProblem(problem: Problem, namedValues: NamedValues = NamedValues.empty): Failed =
      Failed(Some(problem.toString), namedValues)

    def fromThrowable(throwable: Throwable): Failed =
      Failed(Some(throwable.toStringWithCauses))

    protected def make(namedValues: NamedValues): Failed =
      if namedValues.isEmpty then
        failed
      else
        Failed(errorMessage = None, namedValues)

    implicit val jsonEncoder: Encoder.AsObject[Failed] =
      o => JsonObject.fromIterable(
        ("uncatchable" -> (o.uncatchable ? true).asJson) ::
        ("message" -> o.errorMessage.asJson) ::
        o.namedValues.nonEmpty.thenList("namedValues" -> o.namedValues.asJson))

    implicit val jsonDecoder: Decoder[Failed] =
      c => for
        uncatchable <- c.getOrElse[Boolean]("uncatchable")(false)
        errorMessage <- c.get[Option[String]]("message")
        namedValues <- c.getOrElse[NamedValues]("namedValues")(Map.empty)
      yield Failed(errorMessage, namedValues, uncatchable)

  final case class TimedOut(outcome: OrderOutcome.Completed)
  extends OrderOutcome:
    def show = s"TimedOut($outcome)"
    override def toString = "⚠️ " + show

  final case class Killed(outcome: OrderOutcome.Completed)
  extends OrderOutcome:
    def show = s"Killed($outcome)"
    override def toString = "⚠️ " + show

  @TestOnly
  def killed(signal: ProcessSignal): Killed =
    Killed(OrderOutcome.failedWithSignal(signal))

  @TestOnly
  val killedInternal: Killed =
    Killed(OrderOutcome.Failed(Some("Canceled")))

  /** No response from job - some other error has occurred. */
  final case class Disrupted(reason: Disrupted.Reason, uncatchable: Boolean = false)
  extends OrderOutcome, NotSucceeded:
    def show = s"Disrupted($reason)"
    override def toString = "💥 " + (uncatchable ?? "uncatchable ") + show

  object Disrupted:
    def apply(problem: Problem): Disrupted =
      apply(problem, uncatchable = false)

    def apply(problem: Problem, uncatchable: Boolean): Disrupted =
      Disrupted(Other(problem), uncatchable)

    sealed trait Reason:
      def problem: Problem

    implicit val jsonEncoder: Encoder.AsObject[Disrupted] =
      o => JsonObject(
        "reason" -> o.reason.asJson,
        "uncatchable" -> (o.uncatchable ? true).asJson)

    implicit val jsonDecoder: Decoder[Disrupted] =
      c => for
        problem <- c.get[Disrupted.Reason]("reason")
        uncatchable <- c.getOrElse[Boolean]("uncatchable")(false)
      yield Disrupted(problem, uncatchable)

    /** Indicates that the Engine should restart the job.
     * @see WorkflowJob#isNotRestartable.
     */
    final case class ProcessLost private[OrderOutcome](problem: Problem) extends Reason
    object ProcessLost:
      private val jsonEncoder: Encoder.AsObject[ProcessLost] =
        o => JsonObject("problem" -> ((o.problem != ProcessLostDueToUnknownReasonProblem) ? o.problem).asJson)

      private val jsonDecoder: Decoder[ProcessLost] =
        c => for problem <- c.getOrElse[Problem]("problem")(ProcessLostDueToUnknownReasonProblem) yield
          ProcessLost(problem)

      implicit val jsonCodec: Codec.AsObject[ProcessLost] =
        Codec.AsObject.from(jsonDecoder, jsonEncoder)

    final case class Other(problem: Problem) extends Reason

    object Reason:
      implicit val jsonCodec: TypedJsonCodec[Reason] = TypedJsonCodec(
        Subtype[ProcessLost](aliases = Seq("JobSchedulerRestarted")),
        Subtype(deriveCodec[Other]))

  def processLost(problem: Problem): Disrupted =
    Disrupted(ProcessLost(problem))

  sealed trait NotSucceeded extends OrderOutcome:
    def uncatchable: Boolean

  object NotSucceeded:
    implicit val jsonCodec: TypedJsonCodec[NotSucceeded] = TypedJsonCodec(
      Subtype[Failed],
      Subtype[Disrupted])

  private val typedJsonCodec = TypedJsonCodec[OrderOutcome](
    Subtype[Succeeded],
    Subtype(Caught),
    Subtype[Failed],
    Subtype(deriveCodec[TimedOut]),
    Subtype(deriveCodec[Killed]),
    Subtype[Disrupted])

  private val predefinedRC0SucceededJson: JsonObject =
    TypedJsonCodec.typeField[Succeeded] +: Succeeded.returnCode0.asJsonObject

  implicit val jsonEncoder: Encoder.AsObject[OrderOutcome] = outcome =>
    if outcome eq Succeeded.returnCode0 then
      predefinedRC0SucceededJson
    else
      typedJsonCodec.encodeObject(outcome)

  implicit val jsonDecoder: Decoder[OrderOutcome] =
    typedJsonCodec
