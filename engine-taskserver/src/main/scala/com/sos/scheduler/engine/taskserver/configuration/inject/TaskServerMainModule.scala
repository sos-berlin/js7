package com.sos.scheduler.engine.taskserver.configuration.inject

import akka.actor.{ActorRefFactory, ActorSystem}
import com.google.common.io.Closer
import com.google.inject.{AbstractModule, Provides}
import com.sos.scheduler.engine.common.ClassLoaders._
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersCloser
import com.sos.scheduler.engine.common.scalautil.SideEffect.ImplicitSideEffect
import com.sos.scheduler.engine.common.system.OperatingSystem.isWindows
import com.sos.scheduler.engine.common.utils.JavaResource
import com.sos.scheduler.engine.taskserver.configuration.inject.TaskServerMainModule._
import com.sos.scheduler.engine.taskserver.data.DotnetConfiguration
import com.sos.scheduler.engine.taskserver.dotnet.Jni4netModuleInstanceFactory
import com.sos.scheduler.engine.taskserver.dotnet.api.DotnetModuleInstanceFactory
import com.sos.scheduler.engine.taskserver.moduleapi.ModuleFactoryRegister
import com.sos.scheduler.engine.taskserver.modules.StandardModuleFactories
import com.sos.scheduler.engine.taskserver.modules.dotnet.DotnetModule
import com.typesafe.config.ConfigFactory
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

/**
 * @author Joacim Zschimmer
 */
final class TaskServerMainModule(dotnet: DotnetConfiguration) extends AbstractModule {

  def configure() = {}

  @Provides @Singleton
  private def moduleFactoryRegister(dotnetModuleFactory: DotnetModule.Factory): ModuleFactoryRegister =
    new ModuleFactoryRegister(StandardModuleFactories :+ dotnetModuleFactory)

  @Provides @Singleton
  private def dotnetModuleFactory(): DotnetModule.Factory = {
    val factory = dotnet.adapterDllDirectory match {
      case Some(dir) if isWindows ⇒ new Jni4netModuleInstanceFactory(dir)
      case _ ⇒ DotnetModuleInstanceFactory.Unsupported
    }
    new DotnetModule.Factory(factory, classDllDirectory = dotnet.classDllDirectory)
  }

  @Provides @Singleton
  private def executionContext(o: ActorSystem): ExecutionContext = o.dispatcher

  @Provides @Singleton
  private def actorRefFactory(o: ActorSystem): ActorRefFactory = o

  @Provides @Singleton
  private def actorSystem(closer: Closer): ActorSystem = {
    ActorSystem("TaskServerMain", ConfigFactory.load(currentClassLoader, ConfigurationResource.path)) sideEffect  { o ⇒
      closer.onClose {
        o.shutdown()
        //We want to terminate immediately: o.awaitTermination(ShutdownDuration)
      }
    }
  }

  @Provides @Singleton
  private def closer: Closer = Closer.create()  // Do not use concurrently !!!
}

object TaskServerMainModule {
  private val ConfigurationResource = JavaResource("com/sos/scheduler/engine/taskserver/configuration/TaskServerMain-akka.conf")
}
