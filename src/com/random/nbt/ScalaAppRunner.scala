package com.random.nbt

import java.net.URL
import java.lang.reflect.Method
import java.io.File
import scala.util.Try
import scala.util.Failure
import scala.util.Success

class ScalaAppRunner extends FileUtils {
  def findMainClass(implicit binDir: String) = {

    val validBinDir = if (binDir.endsWith(File.separator)) binDir else binDir + File.separator
    implicit val classLoader = new java.net.URLClassLoader(Array(
        new URL(s"file://$validBinDir")
        ))

    val filesWithMainMethod = scanFilesInDir(binDir, fileName => fileName.endsWith(".class") && containsMain(fileName))
    if (filesWithMainMethod.length == 0)
      println("Error: No files with main class found")
    else if (filesWithMainMethod.length > 1) {
      println("Error: There are many classes with a main method: ")
      filesWithMainMethod foreach (println(_))
    }
  }

  def containsMain(fileName: String)(implicit binDir: String, classLoader: ClassLoader) = {
    val className = getClassNameFromFileName(fileName)
    try {
      val fileClass = classLoader.loadClass(className)
      fileClass.getMethods.find { m => m.getName == "main" }.nonEmpty
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
