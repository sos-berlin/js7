package js7.base.catsutils

import js7.base.log.{Log4j, Logger}
import js7.base.utils.ScalaUtils.syntax.RichJavaClass

private transparent trait InitializeLogger:

  protected def productName: String =
    getClass.shortClassName.stripSuffix("Main")

  Log4j.earlyInitializeForProduction()
  Logger.initialize(productName)
