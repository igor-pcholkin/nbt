package com.random.nbt

import scala.annotation.tailrec
import java.io.File

trait FileUtils {
  def scanFilesInDir(sourceDir: String, fileFilter: String => Boolean): List[String] = {
    @tailrec
    def scanFilesInDir(addedFiles: List[String], nonProcessedDirs: List[String]): List[String] = {
      if (nonProcessedDirs.isEmpty) {
        addedFiles
      } else {
        val sourceDir = nonProcessedDirs.head
        val srcDirEntries = new File(sourceDir).list().toList map (createAbsolutePath(sourceDir, _))
        val (srcDirDirs, srcDirFiles) = srcDirEntries.partition ( new File(_).isDirectory )
        val scalaFiles = srcDirFiles filter fileFilter
        scanFilesInDir(addedFiles ++ scalaFiles, nonProcessedDirs.tail ++ srcDirDirs)
      }
    }
    scanFilesInDir(Nil, List(sourceDir))
  }

  def createAbsolutePath(sourceDir: String, fileName: String) = {
    sourceDir + (if (sourceDir.endsWith(File.separator)) "" else File.separator) + fileName
  }

}
