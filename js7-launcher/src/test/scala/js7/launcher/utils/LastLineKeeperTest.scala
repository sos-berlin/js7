package js7.launcher.utils

import js7.base.test.OurTestSuite

final class LastLineKeeperTest extends OurTestSuite:

  private val lineLengthMax = 4096

  "Empty" in:
    val lastLineKeeper = LastLineKeeper(lineLengthMax)
    assert(lastLineKeeper.lastLine == None)
    assert(lastLineKeeper.testLastErrLine == "")

  "Single line" in:
    val lastLineKeeper = LastLineKeeper(lineLengthMax)
    lastLineKeeper.put("LAST\n")
    assert(lastLineKeeper.lastLine == Some("LAST"))
    assert(lastLineKeeper.testLastErrLine == "LAST\n")

  "Two line" in:
    val lastLineKeeper = LastLineKeeper(lineLengthMax)
    lastLineKeeper.put("ONE\n")
    lastLineKeeper.put("LAST\n")
    assert(lastLineKeeper.lastLine == Some("LAST"))
    assert(lastLineKeeper.testLastErrLine == "LAST\n")

  "Two line in same chunk" in:
    val lastLineKeeper = LastLineKeeper(lineLengthMax)
    lastLineKeeper.put("ONE\nLAST\n")
    assert(lastLineKeeper.lastLine == Some("LAST"))
    assert(lastLineKeeper.testLastErrLine == "LAST\n")

  "No line end" in:
    val lastLineKeeper = LastLineKeeper(lineLengthMax)
    lastLineKeeper.put("ONE\nLAST")
    assert(lastLineKeeper.lastLine == Some("LAST"))
    assert(lastLineKeeper.testLastErrLine == "LAST")

  "Line ends in separate chunk" in:
    val lastLineKeeper = LastLineKeeper(lineLengthMax)
    lastLineKeeper.put("ONE\nLAST")
    lastLineKeeper.put("\n")
    assert(lastLineKeeper.lastLine == Some("LAST"))
    assert(lastLineKeeper.testLastErrLine == "LAST\n")

  "Line is split across multiple chunks" in:
    val lastLineKeeper = LastLineKeeper(lineLengthMax)
    lastLineKeeper.put("ONE")
    lastLineKeeper.put("\nL")
    lastLineKeeper.put("A")
    lastLineKeeper.put("ST")
    assert(lastLineKeeper.lastLine == Some("LAST"))
    assert(lastLineKeeper.testLastErrLine == "LAST")

  "Carriage return" in:
    val lastLineKeeper = LastLineKeeper(lineLengthMax)
    lastLineKeeper.put("LAST\r\n")
    assert(lastLineKeeper.lastLine == Some("LAST"))
    assert(lastLineKeeper.testLastErrLine == "LAST\r\n")

  "Split carriage return" in:
    val lastLineKeeper = LastLineKeeper(lineLengthMax)
    lastLineKeeper.put("LAST\r")
    lastLineKeeper.put("\n")
    assert(lastLineKeeper.lastLine == Some("LAST"))
    assert(lastLineKeeper.testLastErrLine == "LAST\r\n")

  "Long line" in:
    val lastLineKeeper = LastLineKeeper(lineLengthMax)
    lastLineKeeper.put("x" * (lineLengthMax + 1) + "\n")
    assert(lastLineKeeper.lastLine == Some("x" * (lineLengthMax - 3) + "..."))
    assert(lastLineKeeper.testLastErrLine == "x" * (lineLengthMax + 1))

  "Long line split accross chunks" in:
    val lastLineKeeper = LastLineKeeper(lengthLengthMax = 23)
    val xxx = ('0' to '9').mkString
    val yyy = ('a' to 'z').mkString
    val zzz = ('A' to 'K').mkString
    assert(xxx.length == 10 && yyy.length == 26 && zzz.length == 11)
    lastLineKeeper.put(xxx)
    lastLineKeeper.put(yyy)
    lastLineKeeper.put(zzz)
    assert(lastLineKeeper.lastLine == Some((xxx + yyy).take(23 - 3) + "..."))
    assert(lastLineKeeper.testLastErrLine == (xxx + yyy).take(23 + 1))
