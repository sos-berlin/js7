package js7.base.utils

import js7.base.data.ByteArray
import js7.base.test.OurAsyncTestSuite

final class LineSplitterTest extends OurAsyncTestSuite:

  "empty" in:
    val toLines = new LineSplitter[ByteArray]
    assert(toLines(ByteArray.empty).isEmpty)

  "one line" in:
    val toLines = new LineSplitter[ByteArray]
    assert(toLines(ByteArray("line\n")) == List(ByteArray("line\n")))

  "more lines" in:
    val toLines = new LineSplitter[ByteArray]
    assert(toLines(ByteArray("line1\nline2\nline3\n")) ==
      List(ByteArray("line1\n"), ByteArray("line2\n"), ByteArray("line3\n")))

  "split lines" - {
    lazy val toLines = new LineSplitter[ByteArray]

    "part 1" in:
      assert(toLines(ByteArray("line-begin")) == Nil)

    "part 2" in:
      assert(toLines(ByteArray("...middle")) == Nil)

    "part 3" in:
      assert(toLines(ByteArray("...end\nline2")) == List(ByteArray("line-begin...middle...end\n")))

    "line2, line3" in:
      assert(toLines(ByteArray("\nline3\n")) == List(ByteArray("line2\n"), ByteArray("line3\n")))

    "line4" in:
      assert(toLines(ByteArray("line4\n")) == List(ByteArray("line4\n")))
  }
