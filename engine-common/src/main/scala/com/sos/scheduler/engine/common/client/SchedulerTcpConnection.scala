package com.sos.scheduler.engine.common.client

import SchedulerTcpConnection._
import com.google.common.base.Charsets.UTF_8
import com.google.common.io.ByteStreams
import com.sos.scheduler.engine.common.scalautil.xmls.SafeXML
import java.io.{StringWriter, InputStream, ByteArrayInputStream}
import java.net.{SocketAddress, Socket}

final class SchedulerTcpConnection(schedulerAddress: SocketAddress)
    extends AutoCloseable {

  private val socket = new Socket
  private lazy val outputStream = socket.getOutputStream
  private lazy val inputStream = socket.getInputStream

  def connect(): Unit = {
    require(!socket.isConnected)
    socket.connect(schedulerAddress)
  }

  def close(): Unit = {
    socket.close()
  }

  def sendAndReceiveXML(commandXml: xml.Elem): xml.Elem =
    SafeXML.load(new ByteArrayInputStream(sendAndReceiveBytes(xmlToBytes(commandXml))))

  def sendAndReceiveBytes(data: Array[Byte]): Array[Byte] = {
    outputStream.write(data)
    ByteStreams.toByteArray(newResponseInputStream())
  }

  def newResponseInputStream() = new InputStream {
    private var eof = false

    def read() =
      if (eof) -1
      else
        inputStream.read() match {
          case '\u0000' => eof = true; -1
          case c => c
        }
  }
}


private object SchedulerTcpConnection {
  private def xmlToBytes(e: xml.Elem): Array[Byte] = {
    val writer = new StringWriter
    xml.XML.write(writer, e, enc = UTF_8.name, xmlDecl = true, doctype = null)
    writer.toString.getBytes(UTF_8.name)
  }
}
