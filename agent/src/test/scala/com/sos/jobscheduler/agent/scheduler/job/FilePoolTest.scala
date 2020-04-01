package com.sos.jobscheduler.agent.scheduler.job

import com.sos.jobscheduler.common.scalautil.FileUtils.syntax._
import com.sos.jobscheduler.common.system.FileUtils.temporaryDirectory
import com.sos.jobscheduler.data.job.JobKey
import java.nio.file.Files.exists
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class FilePoolTest extends FreeSpec
{
  private val filePool = new FilePool(JobKey.forTest, temporaryDirectory)
  private var a: FilePool.FileSet = null
  private var b: FilePool.FileSet = null
  private var c: FilePool.FileSet = null

  "get" in {
    a = filePool.get()
    b = filePool.get()
    c = filePool.get()
    assert(a != b)
    assert(a != c)
    assert(b != c)
  }

  //"underflow" in {
  //  intercept[IllegalStateException] {
  //    filePool.get()
  //  }
  //}

  "release" in {
    b.shellReturnValuesProvider.file := "TEST"
    filePool.release(b)
    val b2 = filePool.get()
    assert(b2 eq b)
    assert(b.shellReturnValuesProvider.file.contentString.isEmpty)
    filePool.release(b2)
  }

  "LIFO" in {
    filePool.release(c)
    val c2 = filePool.get()
    assert(c2 eq c)
    filePool.release(c)
  }

  "close" in {
    filePool.close()
    for (o <- Array(a, b, c)) {
      assert(!exists(o.shellReturnValuesProvider.file))
    }
  }
}
