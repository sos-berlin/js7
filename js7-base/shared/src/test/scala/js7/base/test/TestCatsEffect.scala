package js7.base.test

import cats.effect.SyncIO
import cats.effect.unsafe.IORuntime
import cats.syntax.option.*
import java.nio.charset.StandardCharsets.UTF_8
import java.util.Base64
import js7.base.catsutils.OurIORuntime
import js7.base.configutils.Configs.HoconStringInterpolator
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

  private val _ioRuntime = Atomic(none[Allocated[SyncIO, IORuntime]])
  private var afterAllMayBeCalled = false

  /** For tests that check the thread where blocking operations are executed. */
  final lazy val blockingThreadNamePrefix: String =
    // Suffix will be "-blocking-N" or "-compute-blocker-N"
    if OurIORuntime.useCommonIORuntime then
      OurIORuntime.commonThreadPrefix
    else
      getClass.shortClassName

  protected final lazy val ioRuntime: IORuntime =
    if !afterAllMayBeCalled then
      throw new IllegalStateException("IORuntime used, but beforeAll() has not yet executed")
    else if OurIORuntime.useCommonIORuntime1 then
      OurIORuntime.commonIORuntime
    else
      val allocated =
        OurIORuntime.resource[SyncIO](
            getClass.shortClassName,
            config"""js7.thread-pools.compute.threads = 1/1""")
          .toAllocated
          .unsafeRunSync()
      _ioRuntime := allocated.some
      allocated.allocatedThing

  override protected def beforeAll(): Unit =
    afterAllMayBeCalled = true
    super.beforeAll()

  override protected def afterAll(): Unit =
    try for release <- _ioRuntime.get.map(_.release) do release.unsafeRunSync()
    finally super.afterAll()

  protected def executionContext: ExecutionContext =
    ioRuntime.compute


object TestCatsEffect:
  /** Make a seed for Cats Effect TestControl. */
  def toSeed(number: Long) =
    ByteArray(
      Base64.getEncoder.encode(number.toString.getBytes(UTF_8))
    ).utf8String
