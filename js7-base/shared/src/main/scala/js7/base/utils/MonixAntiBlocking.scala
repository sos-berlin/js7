package js7.base.utils

import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
object MonixAntiBlocking:
  /** To avoid blocking in the following case.
    * - ExecutionContext is a Monix-3.0.0-RC2 Scheduler (TrampolineExecutionContext)
    * - -Dscala.concurrent.context.numThreads=1
    * - Calling Unmarshal(httpResponse).to[X]
    * - Pekko blocks in Await.ready() (after actorOf and ask), waiting for ask response
    * - After 20s, actor.creation-timeout kicks in.
    */
  def executeOn[A](ec: ExecutionContext)(future: ExecutionContext => Future[A]): Future[A] =
    Future(future(ec))(using ec).flatten
