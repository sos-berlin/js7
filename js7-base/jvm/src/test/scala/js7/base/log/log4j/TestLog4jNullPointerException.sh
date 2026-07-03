#!/bin/sh

tmpDir="$(mktemp -d)"
cd "$tmpDir"
echo "Using temporary directory $tmpDir"

scala - <<< '
//> using dep org.apache.logging.log4j:log4j-core:2.26.0
//> using dep com.lmax:disruptor:3.4.4

import java.lang.System.setProperty
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

val config =
  """<?xml version="1.0" encoding="UTF-8"?>
    |<configuration status="WARN">
    |  <appenders>
    |    <randomAccessFile
    |      name="file"
    |      fileName="/tmp/TestLog4jNullPointerException.log"
    |      append="false"
    |      immediateFlush="false"/>
    |  </appenders>
    |
    |  <loggers>
    |    <root level="trace">
    |      <appenderRef level="trace" ref="file"/>
    |    </root>
    |  </loggers>
    |</configuration>
    |""".stripMargin

final class MyException extends Exception:
  private var counter = 0
  override def hashCode =
    counter += 1
    counter

val file: Path = Files.createTempFile("TestLog4jNullPointerException-", "xml")
try
  Files.write(file, config.getBytes(UTF_8))
  setProperty("log4j2.configurationFile", file.toString)
  setProperty("log4j2.contextSelector", "org.apache.logging.log4j.core.async.AsyncLoggerContextSelector")
  val logger = org.apache.logging.log4j.LogManager.getLogger("TestLog4jNullPointerException")
  val exception = new MyException
  logger.info("TEST", exception)
finally
  Files.deleteIfExists(file)
'

rm -rf "$tmpDir"
