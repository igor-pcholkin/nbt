package com.random.nbt

import scala.annotation.tailrec
import java.io.File
import java.lang.reflect.Modifier
import java.net.URL
import com.typesafe.scalalogging.LazyLogging
import scalaz._, Scalaz._

trait FileUtils {
  def scanFilesInDir(rootDir: String, fileFilter: String => Boolean): List[String] = {
    @tailrec
    def scanFilesInDir(foundFiles: List[String], nonProcessedDirs: List[String]): List[String] = {
      if (nonProcessedDirs.isEmpty) {
        foundFiles
      } else {
        val sourceDir = nonProcessedDirs.head
        new File(sourceDir).list() match {
          case null => scanFilesInDir(foundFiles, nonProcessedDirs.tail)
          case dirEntryNames =>
            val srcDirEntriesFullNames = dirEntryNames map (createAbsolutePath(sourceDir, _))
            val (srcDirDirs, srcDirFiles) = srcDirEntriesFullNames partition ( new File(_).isDirectory )
            val sutableFiles = srcDirEntriesFullNames filter fileFilter
            scanFilesInDir(foundFiles ++ sutableFiles, nonProcessedDirs.tail ++ srcDirDirs)
        }
      }
    }
    scanFilesInDir(Nil, List(rootDir))
  }

  def getAllDirFiles(srcDir: String) = {
    scanFilesInDir(srcDir, _ => true)
  }

  def getAllSourceFiles(sourceDir: String): List[String] = scanFilesInDir(sourceDir, fileName => fileName.endsWith(".scala"))

  def getAllDirectSourceFiles(sourceDir: String) = {
    new File(sourceDir).list() match
    {
      case null => Array[String]()
      case files => files.collect { case fileName if fileName.endsWith(".scala") =>
        createAbsolutePath(sourceDir, fileName)
      }
    }
  }

  def createAbsolutePath(sourceDir: String, fileName: String) = {
    sourceDir + (if (sourceDir.endsWith(File.separator)) "" else File.separator) + fileName
  }

  def findMainClass(binDir: String): Validation[String, String] = {

    val validBinDir = if (binDir.endsWith(File.separator)) binDir else binDir + File.separator
    implicit val classLoader = new java.net.URLClassLoader(Array(
        new URL(s"file://$validBinDir")
        ))

    val filesWithMainMethod = scanFilesInDir(binDir, fileName => fileName.endsWith(".class") &&
        containsMain(fileName, binDir) == Success(true))
    if (filesWithMainMethod.length == 0) {
      "No files with main class found".failure
    }
    else if (filesWithMainMethod.length > 1) {
      filesWithMainMethod.foldLeft(new StringBuilder("There are many classes with a main method:")) { (errors, file) =>
        errors.append(file)
      }.toString.failure
    } else {
      getClassNameFromFileName(filesWithMainMethod.head, binDir)
    }
  }

  def containsMain(fileName: String, binDir: String)(implicit classLoader: ClassLoader): Validation[String, Boolean] = {
    getClassNameFromFileName(fileName, binDir) flatMap { className =>
      try {
        val fileClass = classLoader.loadClass(className)
        (fileClass.getMethods.find { m => m.getName == "main" && Modifier.isStatic(m.getModifiers) }.nonEmpty).success
      }
      catch {
        case t: Throwable =>
          (s"Load of $className failed: " + t.getMessage).failure
      }
    }
  }

  def getClassNameFromFileName(fileName: String, binDir: String): Validation[String, String] = {
    val fileNamePattern = s"$binDir${File.separator}?([^\\.]+)\\.class".r
    fileName match {
      case fileNamePattern(className) =>
        className.replace(String.valueOf(File.separator), ".").success
      case _ => "class file match".failure
    }
  }

  def getSourceFileNameFromClassName(className: String, srcDir: String) = {
    createAbsolutePath(srcDir, className.replace(".", String.valueOf(File.separator)) + ".scala")
  }

}
