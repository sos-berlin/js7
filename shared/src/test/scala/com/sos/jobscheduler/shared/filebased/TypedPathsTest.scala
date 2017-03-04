package com.sos.jobscheduler.shared.filebased

import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.time.Stopwatch.measureTime
import com.sos.jobscheduler.data.engine2.order.JobPath
import com.sos.jobscheduler.shared.filebased.TypedPaths.{xmlFileToTypedPath, relativeXmlFilePathToTypedPath}
import java.nio.file.Paths
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class TypedPathsTest extends FreeSpec {

  "xmlFileToTypedPath" in {
    val dir = Paths.get("/some/dir")
    assert(xmlFileToTypedPath[JobPath](dir / "folder/test.job.xml", stripDirectory = dir) == JobPath("/folder/test"))
    assert(xmlFileToTypedPath[JobPath](dir / "folder/test.job.xml", stripDirectory = Paths.get(s"$dir/")) == JobPath("/folder/test"))
  }

  "relativeXmlFilePathToTypedPath" in {
    assert(relativeXmlFilePathToTypedPath[JobPath]("""folder\test.job.xml""") == JobPath("/folder/test"))
    assert(relativeXmlFilePathToTypedPath[JobPath]("folder/test.job.xml") == JobPath("/folder/test"))
    intercept[IllegalArgumentException] {
      relativeXmlFilePathToTypedPath[JobPath]("/folder/test.job.xml")
    }
  }

  if (sys.props contains "test.speed")
  "speed" in {
    val dir = Paths.get("/some/dir")
    val path = dir / "folder/test.job.xml"
    for (_ ← 1 to 10) measureTime(100000, "xmlFileToTypedPath") {
      xmlFileToTypedPath[JobPath](path, stripDirectory = dir)
    }
  }
}
