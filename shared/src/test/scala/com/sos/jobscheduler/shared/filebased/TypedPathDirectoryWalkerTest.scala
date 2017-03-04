package com.sos.jobscheduler.shared.filebased

import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.data.engine2.order.{JobPath, JobnetPath}
import com.sos.jobscheduler.data.filebased.TypedPath
import com.sos.jobscheduler.shared.filebased.TypedPathDirectoryWalker.{forEachTypedFile, matchesFile}
import com.sos.jobscheduler.shared.filebased.TypedPathDirectoryWalkerTest._
import java.nio.file.Files.{createDirectories, createTempDirectory}
import java.nio.file.{Path, Paths}
import org.scalatest.FreeSpec
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class TypedPathDirectoryWalkerTest extends FreeSpec {

  "forEachTypedFile" in {
    provideDataDirectory { dataDir ⇒
      val jobDir = dataDir / "config" / "live"
      val result = mutable.Buffer[(Path, TypedPath)]()
      forEachTypedFile(jobDir, Set(JobPath, JobnetPath)) { (path, typedPath) ⇒
        result += path → typedPath
      }
      assert(result.toSet == Set(
        (jobDir / "test.job.xml") → AJobPath,
        (jobDir / "test.job_chain.xml") → AJobnetPath,
        (jobDir / "folder" / "test.job.xml") → BJobPath))
    }
  }

  "matchesFile" in {
    assert(matchesFile(JobPath, Paths.get("/dir/TEST.job.xml")))
    assert(!matchesFile(JobPath, Paths.get("/dir/TEST.JOB.XML")))
    assert(!matchesFile(JobPath, Paths.get("/dir/TEST.job_chain.xml")))
  }
}

object TypedPathDirectoryWalkerTest {
  val AJobPath = JobPath("/test")
  val BJobPath = JobPath("/folder/test")
  val AJobnetPath = JobnetPath("/test")

  def provideDataDirectory[A](body: Path ⇒ A): A = {
    val dataDir = createTempDirectory("test-")
    val dir = dataDir / "config" / "live"
    val subdir = dir / "folder"
    createDirectories(subdir)
    createDirectories(dir / "ignored.job.xml")
    (dir / "test.job.xml").xml = <job/>
    (dir / "test.job_chain.xml").xml = <job_chain/>
    (subdir / "test.job.xml").xml = <job/>
    (subdir / "test.ignored.xml").xml = <ignored/>
    try body(dataDir)
    finally deleteDirectoryRecursively(dataDir)
  }
}
