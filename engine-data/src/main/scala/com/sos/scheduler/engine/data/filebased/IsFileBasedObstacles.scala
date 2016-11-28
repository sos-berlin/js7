package com.sos.scheduler.engine.data.filebased

/**
  * @author Joacim Zschimmer
  */
trait IsFileBasedObstacles {

  def fileBasedObstacles: Set[FileBasedObstacle]

  override def toString = fileBasedObstacles.mkString(getClass.getSimpleName + "(", ", ", ")")
}
