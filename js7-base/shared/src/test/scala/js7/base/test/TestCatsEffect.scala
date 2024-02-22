package js7.base.test

import cats.effect.SyncIO
import cats.effect.unsafe.IORuntime
import cats.syntax.option.*
import java.nio.charset.StandardCharsets.UTF_8
import java.util.Base64
import java.util.concurrent.locks.ReentrantLock
import js7.base.catsutils.{Js7IORuntime, OwnIORuntime}
import js7.base.data.ByteArray
import js7.base.test.TestCatsEffect.*
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import js7.base.utils.{Allocated, Atomic}
import org.scalatest.{BeforeAndAfterAll, Suite}
import scala.concurrent.ExecutionContext

trait TestCatsEffect extends BeforeAndAfterAll:
  this: Suite =>

  private val lock = new ReentrantLock
  private val _ioRuntime = Atomic(none[Allocated[SyncIO, IORuntime]])
  private var afterAllMayBeCalled = false

  protected final lazy val ioRuntime: IORuntime =
    if !afterAllMayBeCalled then
      throw new IllegalStateException("IORuntime used but beforeAll() has not yet executed")
    else if useCommonIORuntime then
      Js7IORuntime.ioRuntime
    else
      lock.lockInterruptibly()
      try
        val allocated =
          OwnIORuntime
            .resource[SyncIO](name = getClass.shortClassName)
            .toAllocated
            .unsafeRunSync()
        _ioRuntime := allocated.some
        allocated.allocatedThing
      finally
        lock.unlock()

  override protected def beforeAll(): Unit =
    afterAllMayBeCalled = true
    super.beforeAll()

  override protected def afterAll(): Unit =
    try for release <- _ioRuntime.get.map(_.release) do release.unsafeRunSync()
    finally super.afterAll()

  protected def executionContext: ExecutionContext =
    ioRuntime.compute


object TestCatsEffect:

  val useCommonIORuntime = sys.props.contains("js7.test.commonIORuntime")
    || sys.props.contains("test.speed")

  /** Make a seed for Cats Effect TestControl. */
  def toSeed(number: Long) =
    ByteArray(
      Base64.getEncoder.encode(number.toString.getBytes(UTF_8))
    ).utf8String
