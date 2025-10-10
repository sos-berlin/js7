package js7.base.test

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, SyncIO}
import cats.syntax.option.*
import java.nio.charset.StandardCharsets.UTF_8
import java.util.Base64
import js7.base.catsutils.CatsEffectExtensions.run
import js7.base.catsutils.OurIORuntime
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.data.ByteArray
import js7.base.metering.CallMeterLoggingService
import js7.base.thread.CatsBlocking.unsafeRunSyncX
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import js7.base.utils.Tests.isIntelliJIdea
import js7.base.utils.{Allocated, Atomic, Missing}
import org.scalatest.{BeforeAndAfterAll, Suite}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt

trait TestCatsEffect extends BeforeAndAfterAll:
  this: Suite =>

  private val _ioRuntime = Atomic(none[Allocated[SyncIO, IORuntime]])
  private var afterAllMayBeCalled = false
  private var stopCallMeterService: IO[Unit] | Missing = Missing

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
    else if false then
      // Experimental
      val rt = IORuntime.global
      OurIORuntime.register[SyncIO](rt, label = getClass.shortClassName)
        .allocated.run()
      rt
    else
      val allocated =
        OurIORuntime.resource[SyncIO](
            getClass.shortClassName,
            config"""js7.thread-pools.compute.threads = 1/1""")
          .toAllocated.run()
      _ioRuntime := allocated.some

      if isIntelliJIdea then
        stopCallMeterService =
          CallMeterLoggingService.resource(logEvery = 1.minute).void
            .allocated.map(_._2)
            .unsafeRunSyncX()(using allocated.allocatedThing)

      allocated.allocatedThing

  override protected def beforeAll(): Unit =
    afterAllMayBeCalled = true
    super.beforeAll()

  override protected def afterAll(): Unit =
    try
      stopCallMeterService.foreach: stop =>
        stop.unsafeRunSyncX()(using ioRuntime)
      _ioRuntime.get.map(_.release).foreach(_.run())
    finally
      super.afterAll()

  protected def executionContext: ExecutionContext =
    ioRuntime.compute


object TestCatsEffect:
  /** Make a seed for Cats Effect TestControl. */
  def toSeed(number: Long) =
    ByteArray(
      Base64.getEncoder.encode(number.toString.getBytes(UTF_8))
    ).utf8String
