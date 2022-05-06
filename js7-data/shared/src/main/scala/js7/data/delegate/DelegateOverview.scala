package js7.data.delegate

import js7.base.system.SystemInformation
import js7.base.time.Timestamp
import js7.data.system.JavaInformation

trait DelegateOverview {
  def version: String
  def buildId: String
  def startedAt: Timestamp
  def isTerminating: Boolean
  def system: SystemInformation
  def java: JavaInformation
}
