package js7.base.utils

import cats.effect.Resource
import js7.base.monixutils.MonixBase.syntax._
import js7.base.time.ScalaTime._
import monix.catnap.MVar
import monix.eval.Task

object LockResource
{
  private val warnTimeouts = Seq(3.s, 7.s, 10.s)

  def apply(name: String): Resource[Task, Unit] = {
    val lock = MVar[Task].of(()).memoize

    def acquire = lock
      .flatMap(mvar =>
        mvar.tryTake
          .flatMap {
            case None =>
              scribe.debug(s"Waiting for '$name' lock")
              mvar.take
                .whenItTakesLonger(warnTimeouts)(duration =>
                  scribe.info(s"Still waiting for '$name' lock for ${duration.pretty} ..."))
            case Some(()) =>
              Task.unit
          })

    Resource.make(acquire)(_ => lock.flatMap(_.put(())))
  }
}
