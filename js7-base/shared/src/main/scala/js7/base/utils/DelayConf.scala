package js7.base.utils

import cats.data.NonEmptySeq
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import scala.concurrent.duration.*

final case class DelayConf(
  delays: NonEmptySeq[FiniteDuration],
  resetWhen: FiniteDuration):

  override def toString = s"DelayConf($argString)"

  def argString: String =
    delays.toSeq.view.map(_.pretty).mkString(" ") +
      ((resetWhen != FiniteDuration.MaxValue) ?? " resetWhen=%s".format(resetWhen.pretty) )

object DelayConf:
  def apply(delays: NonEmptySeq[FiniteDuration]): DelayConf =
    DelayConf(delays, delays.last)

  def maybe(delays: Seq[FiniteDuration]): Option[DelayConf] =
    for delays <- NonEmptySeq.fromSeq(delays) yield
      DelayConf(delays)
