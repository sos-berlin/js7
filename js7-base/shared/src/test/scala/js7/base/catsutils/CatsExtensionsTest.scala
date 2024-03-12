package js7.base.catsutils

import cats.effect.SyncIO
import js7.base.catsutils.CatsExtensions.*
import org.scalatest.freespec.AnyFreeSpec
import scala.util.{Failure, Success, Try}

final class CatsExtensionsTest extends AnyFreeSpec:

  "tryIt Right" in:
    assert(SyncIO(7).tryIt.unsafeRunSync() == Success(7))

  "tryIt Left" in:
    val e = new Exception
    assert(SyncIO.raiseError(e).tryIt.unsafeRunSync() == Failure(e))

  "untry Success" in:
    val success: Try[7] = Success(7)
    assert(SyncIO(success).untry.unsafeRunSync() == 7)

  "untry Failure" in:
    val e = new Exception
    val failure: Try[7] = Failure(e)
    assert(SyncIO(failure).untry.attempt.unsafeRunSync() == Left(e))
