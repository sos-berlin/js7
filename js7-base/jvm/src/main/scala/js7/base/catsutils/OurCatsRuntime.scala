//package js7.base.catsutils
//
//import cats.effect.unsafe.{IORuntime, IORuntimeConfig}
//import js7.base.catsutils.OurCatsRuntime.*
//import js7.base.log.Logger
//import js7.base.utils.ScalaUtils.*
//import js7.base.utils.ScalaUtils.syntax.*
//
//object OurCatsRuntime:
//  private val logger = Logger[this.type]
//
//  private def apply(): IORuntime = {
//    IORuntime(
//      compute = newExecuteContext(),
//      blocking = newExecuteContext(),
//      scheduler = IORuntime.defaultScheduler,
//      shutdown = () => {}/*FIXME How to shutdown?*/,
//      IORuntimeConfig())
//  }
//
//  private def newExecuteContext() =
//    IORuntime.createBatchingMacrotaskExecutor(
//      reportFailure = t => logger.error(s"Thread failed with ${t.toStringWithCauses}", t))
