package com.random.nbt

import java.io.File
import java.io.PrintWriter
import scala.io.Source
import scala.collection.mutable.Map

object RecencyCache extends FileUtils {
  val projectDir = InternalCallHandler.getProjectDir.get
  val cacheFile = new File(createAbsolutePath(projectDir, ".nbt-cache"))

  val (cachedSrcFileEntries, cachedBinFileEntries, cachedSrcDependencies) =
    if (exists()) read() else (Map[String, Long](), Map[String, Long](), Map[String, Seq[String]]())

  def exists() = cacheFile.exists()

  def read() = {
    println(s"Reading $cacheFile")
    val lines = Source.fromFile(cacheFile).getLines().toList
    val (srcEntries, binEntries) = parseModificationEntries(lines)
    val cachedSrcDependencies = parseSrcDependencies(lines)
    (getCacheEntryMap(srcEntries), getCacheEntryMap(binEntries), cachedSrcDependencies)
  }

  private def parseSrcDependencies(lines: List[String]): scala.collection.mutable.Map[String, Seq[String]] = {
    Map() ++ ((lines collect {
      case line if line.contains("->") =>
        val depParts = line.split("->") map (_.trim)
        depParts(0) -> depParts(1)
    }).toSeq.groupBy { e => e._1 }) map { e => (e._1, (e._2 map { kv => kv._2 })) }
  }

  private def parseModificationEntries(lines: List[String]) = {
    (lines collect {
      case line if line.contains(":") =>
        line.split(":") map (_.trim)
    }) partition { parts => parts(0) == "src" }
  }

  private def getCacheEntryMap(entries: List[Array[String]]) = {
    Map() ++ (entries map { parts => parts(1) -> parts(2).toLong })
  }

  def refresh(allSrcFiles: Option[Seq[String]] = None, allBinFiles: Option[Seq[String]] = None,
      allSrcDependencies: Option[Map[String, Seq[String]]] = None) = {
    val srcFiles = allSrcFiles.getOrElse(getCachedSrcFileEntries)
    val binFiles = allBinFiles.getOrElse(getCachedBinFileEntries)
    val srcDependencies = allSrcDependencies.getOrElse(cachedSrcDependencies)
    create(srcFiles, binFiles, srcDependencies)
  }

  def getCachedSrcFileEntries = cachedSrcFileEntries.keysIterator.toList
  def getCachedBinFileEntries = cachedBinFileEntries.keysIterator.toList

  def create(allSrcDirFiles: Seq[String], binFiles: Seq[String] = Nil, srcDependencies: Map[String, Seq[String]] = Map[String, Seq[String]]()) = {
    println(s"Writing $cacheFile")
    implicit val printWriter  = new PrintWriter(cacheFile)
    updateModificationCacheEntries(cachedSrcFileEntries, allSrcDirFiles)
    updateModificationCacheEntries(cachedBinFileEntries, binFiles)
    writeFileEntries(cachedSrcFileEntries, "src")
    writeFileEntries(cachedBinFileEntries, "bin")
    writeSrcDependencies(srcDependencies)
    printWriter.close()
  }

  def updateModificationCacheEntries(cachedModificationEntries: Map[String, Long], allSrcDirFiles: Seq[String]) = {
    cachedModificationEntries.clear()
    allSrcDirFiles map { fileName =>
      cachedModificationEntries.put(fileName, new File(fileName).lastModified())
    }
  }

  def writeFileEntries(cachedModificationEntries: Map[String, Long], prefix: String)(implicit printWriter: PrintWriter) = {
    cachedModificationEntries map { case (fileName, fileModification) =>
      printWriter.println(s"$prefix: $fileName: $fileModification")
    }
  }

  def writeSrcDependencies(srcDependencies: Map[String, Seq[String]])(implicit printWriter: PrintWriter) = {
    srcDependencies foreach { case (dependencyFile, dependings) =>
      dependings foreach { depending =>
        printWriter.println(s"$dependencyFile -> $depending")
      }
    }
  }

  def getUpdatedAndDependingSrcFiles(srcDir: String) = {
    val allSrcDirFiles = getAllSourceFiles(srcDir)
    if (exists()) {
      val newFiles = getAllSrcFilesNewerThanInCache(allSrcDirFiles)
      if (newFiles.length > 0) {
        refresh(Some(allSrcDirFiles))
      }
      val dependingSrcFiles = getAllDependingSrcFiles(newFiles)
      newFiles ++ dependingSrcFiles
    } else {
      create(allSrcDirFiles)
      allSrcDirFiles
    }
  }

  private def getAllDependingSrcFiles(srcFiles: Seq[String]) = {
    for {
      srcFile <- srcFiles
      depending <- cachedSrcDependencies.getOrElse(srcFile, Nil)
    } yield (depending)
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
