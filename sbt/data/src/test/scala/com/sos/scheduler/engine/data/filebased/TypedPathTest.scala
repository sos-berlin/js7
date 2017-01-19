package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.data.filebased.TypedPathTest._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class TypedPathTest extends FreeSpec {

  "asTyped" in {
    val jobPath = APath("/TEST")
    assert(jobPath.asTyped[APath] eq jobPath)
    assert(BPath("/TEST").asTyped[APath] == jobPath)
    intercept[IllegalArgumentException] {
      APath("/TEST,1").asTyped[BPath]
    }
  }

  "makeAbsolute" in {
    assert(APath.makeAbsolute("a") == APath("/a"))
    assert(APath.makeAbsolute("a/b") == APath("/a/b"))
    intercept[IllegalArgumentException] { APath.makeAbsolute("./b") }
  }
}

private object TypedPathTest {
  final case class APath(string: String) extends TypedPath {
    validate()
    def companion = APath
  }
  object APath extends TypedPath.Companion[APath]

  final case class BPath(string: String) extends TypedPath {
    validate()
    def companion = BPath
  }
  object BPath extends TypedPath.Companion[BPath] {
    override protected[engine] def isCommaAllowed = false
  }
}
