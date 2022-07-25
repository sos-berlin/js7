package js7.data.delegate

import io.circe.generic.semiauto.deriveCodec
import io.circe.{Decoder, Encoder, Json}
import js7.base.annotation.javaApi
import js7.base.circeutils.CirceUtils.deriveConfiguredCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.ScalaUtils.syntax.RichJavaClass

sealed trait DelegateCouplingState

object DelegateCouplingState
{
  final case class Reset(reason: Reset.Reason) extends DelegateCouplingState
  object Reset {
    val fresh = Reset(Fresh)
    val shutdown = Reset(Shutdown)
    val restart = Reset(Restart)
    val byCommand = Reset(ResetCommand)

    sealed trait Reason {
      @javaApi val string = getClass.simpleScalaName
    }

    /** Initially state. */
    case object Fresh extends Reason

    /** Delegate has shutdown properly and lost its state. */
    case object Shutdown extends Reason

    /** Delegate has restarted without proper shutdown. */
    case object Restart extends Reason

    /** Delegate has been reset by command. */
    case object ResetCommand extends Reason

    implicit val jsonCodec: TypedJsonCodec[Reason] = TypedJsonCodec(
      Subtype(Fresh),
      Subtype(Shutdown),
      Subtype(Restart),
      Subtype(ResetCommand))
  }

  case object Coupled extends DelegateCouplingState

  /** Delegate has shutdown properly and probably have kept its state. */
  // COMPATIBLE with v2.3: Also used for Subagent (which loses its state).
  case object ShutDown extends DelegateCouplingState

  final case class Resetting(force: Boolean = false) extends DelegateCouplingState

  private implicit val configuration: Configuration = Configuration.default.withDefaults
  val typedJsonCodec: TypedJsonCodec[DelegateCouplingState] = TypedJsonCodec(
    Subtype(deriveCodec[Reset]),
    Subtype(Coupled),
    Subtype(ShutDown),
    Subtype(deriveConfiguredCodec[Resetting]))

  implicit val jsonEncoder: Encoder[DelegateCouplingState] = typedJsonCodec

  // COMPATIBLE with v2.3
  implicit val jsonDecoder: Decoder[DelegateCouplingState] =
    c => c.get[String](TypedJsonCodec.TypeFieldName)
      .flatMap {
        case "Fresh" => Right(Reset.fresh)
        case "Reset" if c.get[Json]("reason").isLeft => Right(Reset.byCommand)
        case _ => typedJsonCodec.decode(c)
      }
}
