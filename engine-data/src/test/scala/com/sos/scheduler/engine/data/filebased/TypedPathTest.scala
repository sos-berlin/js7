package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.data.folder.FolderPath
import com.sos.scheduler.engine.data.job.JobPath
import com.sos.scheduler.engine.data.jobchain.JobChainPath
import com.sos.scheduler.engine.data.lock.LockPath
import com.sos.scheduler.engine.data.order.OrderKey
import com.sos.scheduler.engine.data.processclass.ProcessClassPath
import com.sos.scheduler.engine.data.schedule.SchedulePath
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class TypedPathTest extends FreeSpec {

  "filenameExtensions" in {
    assert(TypedPath.extensions ==
      Set(".job.xml", ".job_chain.xml", ".lock.xml", ".monitor.xml", ".process_class.xml", ".order.xml", ".schedule.xml"))
  }

  "asTyped" in {
    val jobPath = JobPath("/TEST")
    assert(jobPath.asTyped[JobPath] eq jobPath)
    assert(UnknownTypedPath("/TEST").asTyped[JobPath] == jobPath)
    assert(UnknownTypedPath("/TEST,1").asTyped[OrderKey] == (JobChainPath("/TEST") orderKey "1"))
    intercept[IllegalArgumentException] {
      UnknownTypedPath("/TEST,1").asTyped[JobChainPath]
    }
  }

  "toTypedString" - {
    for ((path, typedString, cppTypedString) ← List[(TypedPath, String, String)](
      (FolderPath("/PATH")      , "Folder:/PATH"      , "folder:/PATH"),
      (JobPath("/PATH")         , "Job:/PATH"         , "job:/PATH"),
      (JobChainPath("/PATH")    , "JobChain:/PATH"    , "job_chain:/PATH"),
      (LockPath("/PATH")        , "Lock:/PATH"        , "lock:/PATH"),
      (OrderKey("/JOBCHAIN,1")  , "Order:/JOBCHAIN,1" , "order:/JOBCHAIN,1"),
      (ProcessClassPath("/PATH"), "ProcessClass:/PATH", "process_class:/PATH"),
      (SchedulePath("/PATH")    , "Schedule:/PATH"    , "schedule:/PATH")))
    {
      path.companion.name in {
        assert(path.toTypedString == typedString)
        assert(TypedPath.fromTypedString(typedString) == path)
        assert(TypedPath.fromCppTypedString(cppTypedString) == path)
      }
    }
  }
}
