package com.sos.jobscheduler.master.javaapi

import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.master.javaapi.MasterRepoReaderTest._
import java.nio.file.Files.{createDirectories, createTempDirectory}
import java.nio.file.Path
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class MasterRepoReaderTest extends FreeSpec {

  "MasterRepoReader Java" in {
    provideDirectory { directory ⇒
      new MasterRepoReaderTester(directory).test()
    }
  }
}

object MasterRepoReaderTest
{
  private def provideDirectory[A](body: Path ⇒ A): A = {
    val dir = createTempDirectory("test-")
    val folder = dir / "folder"
    createDirectories(folder)
    try body(dir)
    finally deleteDirectoryRecursively(dir)
  }
}
