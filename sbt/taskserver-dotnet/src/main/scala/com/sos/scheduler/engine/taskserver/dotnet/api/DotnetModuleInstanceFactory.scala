package com.sos.scheduler.engine.taskserver.dotnet.api

/**
  * @author Joacim Zschimmer
  */
trait DotnetModuleInstanceFactory extends AutoCloseable {

  /**
    * @tparam A [[sos.spooler.IJob_impl]] or a [[sos.spooler.IMonitor_impl]]
    */
  @throws[Exception]
  def newInstance[A](clazz: Class[A], taskContext: TaskContext, reference: DotnetModuleReference): A
}

object DotnetModuleInstanceFactory {
  val Unsupported: DotnetModuleInstanceFactory = new Unsupported(new NotImplementedError(".Net is not supported"))

  def unsupported(newThrowable: ⇒ Throwable): DotnetModuleInstanceFactory = new Unsupported(newThrowable)

  private class Unsupported(newThrowable: ⇒ Throwable) extends DotnetModuleInstanceFactory {
    def newInstance[A](clazz: Class[A], taskContext: TaskContext, reference: DotnetModuleReference) = throw newThrowable

    def close() = {}
  }
}
