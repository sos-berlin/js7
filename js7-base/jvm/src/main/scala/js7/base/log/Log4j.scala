package js7.base.log

import java.lang.reflect.Method
import monix.execution.atomic.AtomicBoolean
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
object Log4j
{
  private lazy val logger = Logger(getClass)
  private val isShutdown = AtomicBoolean(false)

  def setDefaultConfiguration(resource: String): Unit = {
    if (!sys.props.contains("log4j.configurationFile")) {
      val uri = s"classpath:$resource"
      System.setProperty("log4j.configurationFile", uri)
      logger.debug(s"Default log4j.configurationFile=$uri")
    }
  }

  private val shutdownMethod: Try[Method] =
    Try(Class.forName("org.apache.logging.log4j.LogManager"))
      .flatMap(_
        .getMethod("shutdown", classOf[Boolean], classOf[Boolean]) match {
          case null => Failure(new RuntimeException("Missing method org.apache.logging.log4j.LogManager(Boolean, Boolean)"))
          case o => Success(o)
        })

  for (t <- shutdownMethod.failed) logger.warn(t.toString)

  /**
    * Call in case the shutdown hook is disabled in log4j2.xml: &gt;configuration shutdownHook="disable">.
    */
  def shutdown(): Unit =
    if (!isShutdown.getAndSet(true)) {
      for (shutdown <- shutdownMethod) {
        // Log complete timestamp in case of short log timestamp
        logger.info("shutdown\n" + "┄" * 80 + "\n")
        shutdown.invoke(null, false, false)
      }
    }
}
