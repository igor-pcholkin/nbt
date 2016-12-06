package com.random.nbt

import java.io.File
import java.io.PrintWriter
import scala.io.Source

class RecencyCache(projectDir: String) extends FileUtils {
  val cacheFile = new File(createAbsolutePath(projectDir, ".nbt-cache"))

  def exists() = cacheFile.exists()

  def read() = {
    val lines = Source.fromFile(cacheFile).getLines()
    (lines map { line =>
      val entry = line.split(":") map(_.trim)
      (entry(0) -> entry(1).toLong)
    }) toMap
  }

  def recreate(allSrcDirFiles: Seq[String]) = {
    create(allSrcDirFiles)
  }

  def create(allSrcDirFiles: Seq[String]) = {
    val printWriter  = new PrintWriter(cacheFile)
    allSrcDirFiles foreach { fileName =>
      val fileModification = new File(fileName).lastModified()
      printWriter.println(s"$fileName: $fileModification")
    }
    printWriter.close()
  }

  def getAllSrcFilesNewerThanInCache(allSrcDirFiles: Seq[String]) = {
    val cache = read()
    val recentFiles = allSrcDirFiles flatMap { fileName =>
      println(s"Checking $fileName in cache")
      cache.get(fileName) match {
        case Some(modifiedInCache) =>
          val fileToCheck = new File(fileName)
          if (!fileToCheck.exists() || modifiedInCache < fileToCheck.lastModified())
            Some(fileName)
          else
            None
        case None => Some(fileName)
      }
    }
    recentFiles
  }


}
