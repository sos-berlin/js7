package js7.base.log

trait AdHocLogger:
  this: Logger.type =>

  inline def info(inline msg: String)(using src: sourcecode.Enclosing): Unit =
    logger.info(msg)

  inline def debug(inline msg: String)(using src: sourcecode.Enclosing): Unit =
    logger.debug(msg)

  private def logger(using src: sourcecode.Enclosing) =
    Logger(src.value.replace("#", "."))
