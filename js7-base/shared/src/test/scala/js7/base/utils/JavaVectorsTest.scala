package js7.base.utils

import java.nio.charset.StandardCharsets.UTF_8
import js7.base.log.AnsiEscapeCodes.bold
import js7.base.log.Logger
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.RichDeadline
import js7.base.time.Stopwatch.bytesPerSecondString
import js7.base.utils.JavaVectors.{hasJavaIncubatorVectors, scalarIndexOf, vectorIndexOf}
import js7.base.utils.JavaVectorsTest.*
import js7.base.utils.Tests.isIntelliJIdea
import org.scalatest.Assertion
import scala.concurrent.duration.Deadline

final class JavaVectorsTest extends OurTestSuite:

  "scalarIndexOf" in:
    assert(Array[Byte]('a', '*', 'b').scalarIndexOf('*') == 1)
    assert(Array[Byte]('a', '*', 'b').scalarIndexOf('?') == -1)
    assert(Array[Byte]('a', '*', 'b').scalarIndexOf('*', 1) == 1)
    assert(Array[Byte]('a', '*', 'b').scalarIndexOf('*', 2) == -1)
    standardTests(_.scalarIndexOf(_, _, _))

  "vectorIndexOf" - {
    "vectorIndexOf" in:
      assert(Array[Byte]('a', '*', 'b').vectorIndexOf('*') == 1)
      assert(Array[Byte]('a', '*', 'b').vectorIndexOf('?') == -1)
      assert(Array[Byte]('a', '*', 'b').vectorIndexOf('*', 1) == 1)
      assert(Array[Byte]('a', '*', 'b').vectorIndexOf('*', 2) == -1)
      standardTests(_.vectorIndexOf(_, _, _))

      assert(Array.empty[Byte].vectorIndexOf('\n') == -1)
      assert(Array[Byte]('\n').vectorIndexOf('\n') == 0)
      assert(Array[Byte]('\n', 'x').vectorIndexOf('\n') == 0)
      assert(Array[Byte]('x').vectorIndexOf('\n') == -1)
      assert("one\ntwo\nthree".getBytes(UTF_8).vectorIndexOf('\n') == 3)
      assert("0123456789abcdef\n".getBytes(UTF_8).vectorIndexOf('\n') == 16)
      assert(("+" * (1024 * 1024 - 1)).getBytes(UTF_8).vectorIndexOf('\n') == -1)
      assert(("+" * (1024 * 1024 - 2) + '\n').getBytes(UTF_8).vectorIndexOf('\n') == 1024 * 1024 - 2)

    "Speed with small array" - {
      val small = "0123456789abcdef0123456789\n".getBytes(UTF_8)
      "vectorIndexOf_" in:
        speedTest(small, small.length - 1, n = 1000, "small array")(_.vectorIndexOf('\n'))

      "scalarIndexOf" in:
        speedTest(small, small.length - 1, n = 1000, "small array")(_.scalarIndexOf('\n'))
    }

    "Speed with big array" - {
      // Bigger difference between vectorIndexOf and scalarIndexOf if big is bigger then CPU cache
      lazy val big = ("+" * (128 * 1024 * 1024) + "\n").getBytes(UTF_8)

      "vectorIndexOf" in:
        if isIntelliJIdea then
          speedTest(big, big.length - 1, n = 200, "128 MiB array")(_.vectorIndexOf('\n'))

      "scalarIndexOf" in:
        if isIntelliJIdea then
          speedTest(big, big.length - 1, n = 200, "128 MiB array")(_.scalarIndexOf('\n'))
    }

    def speedTest(array: Array[Byte], expectedIndex: Int, n: Int, label: String)
      (indexOf: Array[Byte] => Int)
    : Assertion =
      if !hasJavaIncubatorVectors then
        pending
      else
        val t = Deadline.now
        var i = 0
        while i < n do
          if indexOf(array) != expectedIndex then
            fail()
          i += 1
        logger.info(s"$label ${bold(bytesPerSecondString(t.elapsed, n.toLong * array.length))}")
        succeed
  }

  private def standardTests(indexOf: (Array[Byte], Byte, Int, Int) => Int): Assertion =
    intercept[ArrayIndexOutOfBoundsException]:
      indexOf(Array[Byte]('a', '*', 'b'), '*', -1, 3)

    assert(indexOf(Array[Byte]('a', '*', 'b'), '*', 0, -1) == -1)
    assert(indexOf(Array[Byte]('a', '*', 'b'), '*', 0, 0) == -1)
    assert(indexOf(Array[Byte]('a', '*', 'b'), '*', 0, 1) == -1)
    assert(indexOf(Array[Byte]('a', '*', 'b'), '*', 0, 2) == 1)
    assert(indexOf(Array[Byte]('a', '*', 'b'), '*', 0, 3) == 1)
    assert(indexOf(Array[Byte]('a', '*', 'b'), '*', 0, Int.MaxValue) == 1)

    assert(indexOf(Array[Byte]('a', '*', 'b'), '*', 1, -1) == -1)
    assert(indexOf(Array[Byte]('a', '*', 'b'), '*', 1, 0) == -1)
    assert(indexOf(Array[Byte]('a', '*', 'b'), '*', 1, 1) == -1)
    assert(indexOf(Array[Byte]('a', '*', 'b'), '*', 1, 2) == 1)
    assert(indexOf(Array[Byte]('a', '*', 'b'), '*', 1, 3) == 1)
    assert(indexOf(Array[Byte]('a', '*', 'b'), '*', 1, Int.MaxValue) == 1)

    assert(indexOf(Array[Byte]('a', '*', 'b'), '*', 2, -1) == -1)
    assert(indexOf(Array[Byte]('a', '*', 'b'), '*', 2, 0) == -1)
    assert(indexOf(Array[Byte]('a', '*', 'b'), '*', 2, 1) == -1)
    assert(indexOf(Array[Byte]('a', '*', 'b'), '*', 2, 2) == -1)
    assert(indexOf(Array[Byte]('a', '*', 'b'), '*', 2, 3) == -1)
    assert(indexOf(Array[Byte]('a', '*', 'b'), '*', 2, Int.MaxValue) == -1)

    assert(indexOf(Array[Byte]('a', '*', 'b'), '*', 3, -1) == -1)
    assert(indexOf(Array[Byte]('a', '*', 'b'), '*', 3, 0) == -1)
    assert(indexOf(Array[Byte]('a', '*', 'b'), '*', 3, 1) == -1)
    assert(indexOf(Array[Byte]('a', '*', 'b'), '*', 3, 2) == -1)
    assert(indexOf(Array[Byte]('a', '*', 'b'), '*', 3, 3) == -1)
    assert(indexOf(Array[Byte]('a', '*', 'b'), '*', 3, Int.MaxValue) == -1)


object JavaVectorsTest:
  private val logger = Logger[this.type]
