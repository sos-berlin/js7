package js7.base.test

import cats.effect.unsafe.IORuntime
import java.nio.charset.StandardCharsets.UTF_8
import java.util.Base64
import js7.base.catsutils.OurIORuntime
import js7.base.data.ByteArray
import scala.concurrent.ExecutionContext

trait TestCatsEffect:

  // Or use own IORuntime for each test class ???
  protected final def ioRuntime: IORuntime =
    //IORuntime.global
    OurIORuntime.ioRuntime

  implicit def executionContext: ExecutionContext =
    ioRuntime.compute

  given IORuntime = ioRuntime


object TestCatsEffect:

  def toSeed(number: Long) =
    ByteArray(
      Base64.getEncoder.encode(number.toString.getBytes(UTF_8))
    ).utf8String
