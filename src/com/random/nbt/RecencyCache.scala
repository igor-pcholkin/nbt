package com.random.nbt

import java.io.File
import java.io.PrintWriter
import scala.io.Source

class RecencyCache(projectDir: String) extends FileUtils {
  val cacheFile = new File(createAbsolutePath(projectDir, ".nbt-cache"))

  val (cachedSrcFileEntries, cachedBinFileEntries) = if (exists()) read() else (Map[String, Long](), Map[String, Long]())

  def exists() = cacheFile.exists()

  def read() = {
    val lines = Source.fromFile(cacheFile).getLines()
    val (srcEntries, binEntries) = (lines map { line =>
      line.split(":") map(_.trim)
    }) partition { parts => parts(0) == "src" }
    (getCacheEntryMap(srcEntries), getCacheEntryMap(binEntries))
  }

  private def getCacheEntryMap(entries: Iterator[Array[String]]) = entries map { parts => parts(1) -> parts(2).toLong } toMap

  def refresh(allSrcFiles: Option[Seq[String]] = None, allBinFiles: Option[Seq[String]] = None) = {
    val srcFiles = allSrcFiles.getOrElse(getCachedSrcFileEntries)
    val binFiles = allBinFiles.getOrElse(getCachedBinFileEntries)
    create(srcFiles, binFiles)
  }

  def getCachedSrcFileEntries = cachedSrcFileEntries.keysIterator.toSeq
  def getCachedBinFileEntries = cachedBinFileEntries.keysIterator.toSeq

  def create(allSrcDirFiles: Seq[String], binFiles: Seq[String] = Nil) = {
    val printWriter  = new PrintWriter(cacheFile)
    writeFileEntries(allSrcDirFiles, "src", printWriter)
    writeFileEntries(binFiles, "bin", printWriter)
    printWriter.close()
  }

  def writeFileEntries(fileNames: Seq[String], prefix: String, printWriter: PrintWriter) = {
    fileNames foreach { fileName =>
      val fileModification = new File(fileName).lastModified()
      printWriter.println(s"$prefix: $fileName: $fileModification")
    }
  }

  def getUpdatedSrcFiles(srcDir: String) = {
    val allSrcDirFiles = getAllSourceFiles(srcDir)
    if (!exists()) {
      create(allSrcDirFiles)
      allSrcDirFiles
    } else {
      val newFiles = getAllSrcFilesNewerThanInCache(allSrcDirFiles)
      if (newFiles.length > 0) {
        refresh(Some(allSrcDirFiles))
      }
      newFiles
    }
  }

  private def getAllSrcFilesNewerThanInCache(allSrcDirFiles: Seq[String]) = {
    val recentFiles = allSrcDirFiles flatMap { fileName =>
      println(s"Checking $fileName in cache")
      cachedSrcFileEntries.get(fileName) match {
        case Some(lastModifiedInCache) =>
          if (lastModifiedInCache < new File(fileName).lastModified())
            Some(fileName)
          else
            None
        case None => Some(fileName)
      }
    }
    recentFiles
  }


}
