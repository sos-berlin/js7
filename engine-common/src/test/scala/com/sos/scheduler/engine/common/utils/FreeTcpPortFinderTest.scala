package com.sos.scheduler.engine.common.utils

import FreeTcpPortFinder._
import java.net.ServerSocket
import org.junit.runner.RunWith
import org.scalatest.{FreeSpec, FunSuite}
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
final class FreeTcpPortFinderTest extends FreeSpec {

  "findRandomFreePort" in {
    for (i ← 1 to 1000) {
      findRandomFreeTcpPort(20000 until 30000) should (be >= 20000 and be <= 30000)
    }
  }

  "findRandomFreePort with empty list should throw exception" in {
    val portNumber = findRandomFreeTcpPort(20000 until 30000)
    val socket = new ServerSocket(portNumber, 1)
    intercept[Exception] { findRandomFreeTcpPort(portNumber to portNumber) }
    socket.close()
    findRandomFreeTcpPort(portNumber to portNumber)
  }

  "findRandomFreePort with no available port should throw exception" in {
    val emptyRange = 20000 until 20000
    emptyRange.size shouldEqual 0
    intercept[Exception] { findRandomFreeTcpPort(emptyRange) }
  }

  "findRandomFreePorts return distinct port numbers" in {
    for (i ← 1 to 100) {
      val n = 100
      findRandomFreeTcpPorts(n, 20000 until 21000).distinct should have ('size(n))
    }
  }
}
