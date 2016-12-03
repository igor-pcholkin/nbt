package com.random.nbt

import scala.annotation.tailrec
import java.io.File
import java.lang.reflect.Modifier
import java.net.URL

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

  def findMainClass(implicit binDir: String): Option[String] = {

    val validBinDir = if (binDir.endsWith(File.separator)) binDir else binDir + File.separator
    implicit val classLoader = new java.net.URLClassLoader(Array(
        new URL(s"file://$validBinDir")
        ))

    val filesWithMainMethod = scanFilesInDir(binDir, fileName => fileName.endsWith(".class") && containsMain(fileName))
    if (filesWithMainMethod.length == 0) {
      println("Error: No files with main class found")
      None
    }
    else if (filesWithMainMethod.length > 1) {
      println("Error: There are many classes with a main method: ")
      filesWithMainMethod foreach (println(_))
      None
    } else
      Some(getClassNameFromFileName(filesWithMainMethod.head))
  }

  def containsMain(fileName: String)(implicit binDir: String, classLoader: ClassLoader) = {
    val className = getClassNameFromFileName(fileName)
    try {
      val fileClass = classLoader.loadClass(className)
      fileClass.getMethods.find { m => m.getName == "main" && Modifier.isStatic(m.getModifiers) }.nonEmpty
    }
    catch {
      case t: Throwable =>
        println(s"Load of $className failed: " + t.getMessage)
        false
    }
  }

  def getClassNameFromFileName(fileName: String)(implicit binDir: String) = {
    val fileNamePattern = s"$binDir${File.separator}?([^\\.]+)\\.class".r
    fileName match {
      case fileNamePattern(className) =>
        className.replace(String.valueOf(File.separator), ".")
      case _ => println("Error: class file match")
      ""
    }
  }

}
