package com.sos.scheduler.engine.data.filebaseds

import com.sos.scheduler.engine.data.filebased.{FileBasedType, TypedPath}
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
final class TypedPathRegisterTest extends FreeSpec {

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
        assert(TypedPathRegister.fromTypedString(typedString) == path)
        assert(TypedPathRegister.fromCppTypedString(cppTypedString) == path)
      }
    }
  }

  "path" in {
    assert(TypedPathRegister.fileBasedTypedToCompanion(FileBasedType.Folder      ).apply("/a") == FolderPath("/a"))
    assert(TypedPathRegister.fileBasedTypedToCompanion(FileBasedType.Job         ).apply("/a") == JobPath("/a"))
    assert(TypedPathRegister.fileBasedTypedToCompanion(FileBasedType.JobChain    ).apply("/a") == JobChainPath("/a"))
    assert(TypedPathRegister.fileBasedTypedToCompanion(FileBasedType.Lock        ).apply("/a") == LockPath("/a"))
    assert(TypedPathRegister.fileBasedTypedToCompanion(FileBasedType.ProcessClass).apply("/a") == ProcessClassPath("/a"))
    assert(TypedPathRegister.fileBasedTypedToCompanion(FileBasedType.Order       ).apply("/a,1") == OrderKey("/a", "1"))
    assert(TypedPathRegister.fileBasedTypedToCompanion(FileBasedType.Schedule    ).apply("/a") == SchedulePath("/a"))
  }
}
