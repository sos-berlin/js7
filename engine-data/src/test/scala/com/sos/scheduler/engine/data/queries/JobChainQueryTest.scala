package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.data.folder.FolderPath
import com.sos.scheduler.engine.data.jobchain.JobChainPath
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class JobChainQueryTest extends FreeSpec {

  "All" in {
    val q = JobChainQuery.All
    assert(q == JobChainQuery.Standard(PathQuery("/")))
    assert(q matchesJobChain QueryableJobChain.ForTest(JobChainPath("/a")))
    assert(q matchesJobChain QueryableJobChain.ForTest(JobChainPath("/a/b")))
  }

  "Single JobChainPath" in {
    val q = JobChainQuery.Standard(PathQuery(JobChainPath("/a/b")))
    assert(q == JobChainQuery.Standard(PathQuery("/a/b")))
    assert(!(q matchesJobChain QueryableJobChain.ForTest(JobChainPath("/a"))))
    assert(!(q matchesJobChain QueryableJobChain.ForTest(JobChainPath("/a/b/c"))))
    assert(q matchesJobChain QueryableJobChain.ForTest(JobChainPath("/a/b"), isDistributed = false))
    assert(q matchesJobChain QueryableJobChain.ForTest(JobChainPath("/a/b"), isDistributed = true))
  }

  "isDistributed" in {
    val q = JobChainQuery.Standard(PathQuery(FolderPath("/a")), isDistributed = Some(false))
    assert(q matchesJobChain new QueryableJobChain.ForTest(JobChainPath("/a/b"), isDistributed = false))
    assert(!q.matchesJobChain(new QueryableJobChain.ForTest(JobChainPath("/a/b"), isDistributed = true)))
  }
}
