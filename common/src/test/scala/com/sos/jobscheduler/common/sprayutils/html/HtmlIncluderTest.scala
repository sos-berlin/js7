package com.sos.jobscheduler.common.sprayutils.html

import com.sos.jobscheduler.common.scalautil.FileUtils.implicits.RichPath
import java.nio.file.Files
import java.nio.file.Files.delete
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class HtmlIncluderTest extends FreeSpec {

  "uriToSha224" in {
    val file = Files.createTempFile("test-", ".tmp")
    file.contentString = "Franz jagt im komplett verwahrlosten Taxi quer durch Bayern"
    val hash: String = HtmlIncluder.uriToSha224(file.toUri.toURL)
    //Stopwatch.measureTime(10000, "uriToSha224") {
    //  HtmlIncluder.uriToSha224(file.toUri.toString)
    //}
    delete(file)
    assert(hash == "49b08defa65e644cbf8a2dd9270bdededabc741997d1dadd42026d7b")
  }
}
