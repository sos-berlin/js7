package js7.base.utils

import js7.base.data.ByteArray
import js7.base.test.OurAsyncTestSuite

final class ByteSequenceToLinesStreamTest extends OurAsyncTestSuite:

  "empty" in:
    val toLines = new ByteSequenceToLinesStream[ByteArray]
    for list <- toLines(ByteArray.empty).compile.toList yield
      assert(list.isEmpty)

  "one line" in:
    val toLines = new ByteSequenceToLinesStream[ByteArray]
    for list <- toLines(ByteArray("line\n")).compile.toList yield
      assert(list == List(ByteArray("line\n")))

  "more lines" in:
    val toLines = new ByteSequenceToLinesStream[ByteArray]
    for list <- toLines(ByteArray("line1\nline2\nline3\n")).compile.toList yield
      assert(list == List(ByteArray("line1\n"), ByteArray("line2\n"), ByteArray("line3\n")))

  "split lines" - {
    lazy val toLines = new ByteSequenceToLinesStream[ByteArray]

    "part 1" in:
      for list <- toLines(ByteArray("line-begin")).compile.toList yield
        assert(list == Nil)

    "part 2" in:
      for list <- toLines(ByteArray("...middle")).compile.toList yield
        assert(list == Nil)

    "part 3" in:
      for list <- toLines(ByteArray("...end\nline2")).compile.toList yield
        assert(list == List(ByteArray("line-begin...middle...end\n")))

    "line2, line3" in:
      for list <- toLines(ByteArray("\nline3\n")).compile.toList yield
        assert(list == List(ByteArray("line2\n"), ByteArray("line3\n")))

    "line4" in:
      for list <- toLines(ByteArray("line4\n")).compile.toList yield
        assert(list == List(ByteArray("line4\n")))
  }
