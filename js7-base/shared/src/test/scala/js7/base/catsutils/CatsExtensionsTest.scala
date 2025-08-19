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

  "flatMapSome" in:
    val some: Option[Int] = Some(1)
    val none: Option[Int] = None
    assert(Option(some).flatMapSome(i => Option(i + 1)) == Some(Some(2)))
    assert(SyncIO(some).flatMapSome(i => SyncIO(i + 1)).unsafeRunSync() == Some(2))
    assert(SyncIO(none).flatMapSome(i => SyncIO(i + 1)).unsafeRunSync() == None)

  "flatTapSome" in:
    val some: Option[Int] = Some(1)
    val none: Option[Int] = None
    assert(Option(some).flatTapSome(i => Option(i + 1)) == Some(Some(1)))

    var count = 0
    assert(SyncIO(some).flatTapSome(i => SyncIO { i + 1; count = 1 }).unsafeRunSync() == Some(1))
    assert(count == 1)
    assert(SyncIO(none).flatTapSome(i => SyncIO { i + 1; count = 2 }).unsafeRunSync() == None)
    assert(count == 1)

  "whenM" in:
    var n = 0
    SyncIO(false).whenM(SyncIO(n += 1)).unsafeRunSync()
    assert(n == 0)

    SyncIO(true).whenM(SyncIO(n += 1)).unsafeRunSync()
    assert(n == 1)
