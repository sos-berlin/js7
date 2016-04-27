package com.sos.scheduler.engine.taskserver.configuration.inject

import akka.actor.{ActorRefFactory, ActorSystem}
import com.google.common.io.Closer
import com.google.inject.Provides
import com.sos.scheduler.engine.common.ClassLoaders._
import com.sos.scheduler.engine.common.guice.ScalaAbstractModule
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersCloser
import com.sos.scheduler.engine.common.scalautil.SideEffect.ImplicitSideEffect
import com.sos.scheduler.engine.common.utils.JavaResource
import com.sos.scheduler.engine.taskserver.configuration.inject.TaskServerMainModule._
import com.sos.scheduler.engine.taskserver.dotnet.api.DotnetModuleInstanceFactory
import com.sos.scheduler.engine.taskserver.module.ModuleRegister
import com.sos.scheduler.engine.taskserver.module.dotnet.DotnetModule
import com.typesafe.config.ConfigFactory
import javax.inject.Singleton
import scala.concurrent.ExecutionContext

/**
 * @author Joacim Zschimmer
 */
final class TaskServerMainModule extends ScalaAbstractModule {

  def configure() = {}

  @Provides @Singleton
  private def moduleRegister(dotnetModuleType: DotnetModule.Type): ModuleRegister =
    new ModuleRegister(ModuleRegister.StandardModuleTypes :+ dotnetModuleType)

  @Provides @Singleton
  private def dotnetModuleType(factory: DotnetModuleInstanceFactory): DotnetModule.Type = new DotnetModule.Type(factory)

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
