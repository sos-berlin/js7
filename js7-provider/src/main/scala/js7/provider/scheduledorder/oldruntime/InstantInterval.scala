package js7.provider.scheduledorder.oldruntime

import java.time.*
import js7.base.time.JavaTime.*
import scala.language.implicitConversions

/**
  * Right half-open interval of instants.
  *
  * @author Joacim Zschimmer
  */
final case class InstantInterval(from: Instant, until: Instant):

  def +(duration: Duration) = InstantInterval(from + duration, until + duration)

  override def toString = s"[$from, $until)"

object InstantInterval:
  def apply(from: Instant, duration: Duration): InstantInterval =
    new InstantInterval(from, from + duration)

  implicit def apply(pair: (Instant, Instant)): InstantInterval =
    new InstantInterval(pair._1, pair._2)
