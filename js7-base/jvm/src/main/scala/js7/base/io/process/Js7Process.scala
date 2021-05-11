package js7.base.io.process

import java.io.{InputStream, OutputStream}
import js7.base.io.process.Processes.Pid

trait Js7Process
{
  def pid: Option[Pid]

  def isAlive: Boolean

  def stdin: OutputStream

  def stdout: InputStream

  def stderr: InputStream

  def returnCode: Option[ReturnCode]

  def destroy() : Unit

  def destroyForcibly() : Unit

  def waitFor(): ReturnCode
}
