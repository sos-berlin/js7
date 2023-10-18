package js7.data.value.expression.scopes

import java.io.{IOException, OutputStreamWriter}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files.{createDirectory, newOutputStream}
import java.nio.file.StandardOpenOption.{CREATE_NEW, WRITE}
import java.nio.file.{Files, Path, Paths}
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.system.OperatingSystem.isWindows
import js7.base.utils.AtomicUpdater
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ByteUnits.toKBGB
import js7.base.utils.ScalaUtils.syntax.{RichBoolean, RichThrowable}
import js7.data.value.expression.scopes.FileValueScope.functionName
import js7.data.value.expression.scopes.FileValueState.*
import monix.execution.atomic.Atomic

final class FileValueState(
  private[scopes] val directory: Path)
extends AutoCloseable:

  private val scopeRegister = new ScopeRegister
  private val commonDirectory = Paths.get("0")
  private val commonDirectoryCreated = Atomic(false)
  private val directoryNumber = Atomic(0L)
  private val usedFilenames = Atomic(Set.empty[Path])
  private val uniqueNumber = Atomic(0L)
  private val statistics = new Statistics

  def close(): Unit =
    if commonDirectoryCreated.get() then
      tryDelete(directory.resolve(commonDirectory))
    if statistics.fileCount.get() > 0 then
      logger.info(s"toFile function statistics: $statistics")

  def isEmpty = usedFilenames.get().isEmpty && scopeRegister.isEmpty

  def startScope(scope: FileValueScope): Unit =
    scopeRegister.add(scope)

  def releaseScope(scope: FileValueScope): Unit =
    for scopeFiles <- scopeRegister.get(scope) do
      for file <- scopeFiles.release do
        tryDelete(file)
        usedFilenames.synchronized:
          usedFilenames := usedFilenames.get() - directory.relativize(file)
      scopeRegister.remove(scope)

  def toFile(fileValueScope: FileValueScope, filenamePattern: String, content: String)
  : Checked[Path] =
    Checked.catchNonFatal {
      for relativePath <- toRelativePath(filenamePattern) yield
        val scopeFiles = scopeRegister(fileValueScope)
        val relativeDir = relativePath.getParent
        if relativeDir == commonDirectory then
          commonDirectoryCreated.synchronized:
            if !commonDirectoryCreated.get() then
              createDirectory(directory.resolve(commonDirectory))
              commonDirectoryCreated := true
        else
          val dir = directory.resolve(relativeDir)
          scopeFiles.registerFile(dir)
          createDirectory(dir)
          statistics.directoryCount += 1
        val file = directory.resolve(relativePath)
        autoClosing(new OutputStreamWriter(newOutputStream(file, CREATE_NEW, WRITE), UTF_8)):
          scopeFiles.registerFile(file)
          statistics.fileCount += 1
          _.append(content)
        val size = Files.size(file)
        statistics.totalFileSize += size
        file.toFile.setWritable(false)
        logger.debug(s"$functionName => $file ${toKBGB(size)}")
        file
    }.flatten

  private def toRelativePath(filenamePattern: String): Checked[Path] =
    val filename = resolveStar(filenamePattern)
    var f = commonDirectory.resolve(filename)
    if f.getNameCount != 2 then
      Left(Problem.pure("No directory is allowed in toFile function filenamePattern argument"))
    else Right(
      usedFilenames.synchronized {
        if usedFilenames.get().contains(f) then {
          // Place duplicate filename in its own directory
          val dedicatedDirectory = Paths.get(directoryNumber.incrementAndGet().toString)
          usedFilenames := usedFilenames.get() + dedicatedDirectory
          f = dedicatedDirectory.resolve(filename)
        }
        usedFilenames := usedFilenames.get() + f
        f
      })

  private def resolveStar(filenamePattern: String): String =
    if filenamePattern contains '*' then
      val unique = uniqueNumber.incrementAndGet().toString/*Base64UUID.randomString()*/
      filenamePattern.replace("*", unique)
    else
      filenamePattern

  override def toString = s"FileValueState($statistics)"


object FileValueState:
  private val logger = Logger[this.type]

  private def tryDelete(file: Path): Unit =
    logger.debug(s"Delete $file")
    if isWindows then
      file.toFile.setWritable(true)
    try Files.delete(file)
    catch { case t: IOException =>
      // TODO Delete file later
      logger.warn(s"Delete $file => ${t.toStringWithCauses}")
    }

  private final class ScopeRegister:
    private val scopeToFiles = new AtomicUpdater(Map.empty[FileValueScope, ScopeFiles])

    def apply(scope: FileValueScope): ScopeFiles =
      scopeToFiles.get(scope)

    def get(scope: FileValueScope): Option[ScopeFiles] =
      scopeToFiles.get.get(scope)

    def add(scope: FileValueScope): Unit =
      scopeToFiles.update(_ + (scope -> new ScopeFiles))

    def remove(scope: FileValueScope): Unit =
      scopeToFiles.update(_ - scope)

    def isEmpty = scopeToFiles.get.isEmpty

  private final class ScopeFiles:
    val files = new AtomicUpdater(List.empty[Path])

    def registerFile(file: Path): Unit =
      files.update(file :: _)

    /** Return files in reverse order. */
    def release: Seq[Path] =
      files.getAndSet(Nil)

  private final class Statistics:
    val fileCount = Atomic(0L)
    val directoryCount = Atomic(0L)
    val totalFileSize = Atomic(0L)

    override def toString =
      val fileCount = this.fileCount.get()
      fileCount.toString + " files, " +
        directoryCount.get() + " subdirectories, " +
        toKBGB(totalFileSize.get()) +
        ((fileCount > 0) ?? s", ∅ ${toKBGB(totalFileSize.get() / fileCount)}/file")
