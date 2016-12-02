package com.random.nbt

import java.io.File
import scala.reflect.internal.util.BatchSourceFile
import scala.io.Source
import scala.annotation.tailrec
import java.net.URL

class ScalaCompiler {
  def compile(sourceDir: String) = {
    val classLoader = new java.net.URLClassLoader(Array(
        new URL("file:///Users/igor/Downloads/scala-2.11.8/lib/scala-compiler.jar")
        ))

    val settingsClass = classLoader.loadClass("scala.tools.nsc.Settings")
    val settingsInstance = settingsClass.getConstructor().newInstance().asInstanceOf[AnyRef]

    val reporterClass = classLoader.loadClass("scala.tools.nsc.reporters.ConsoleReporter")
    val reporterInstance = reporterClass.getConstructor(settingsClass).newInstance(settingsInstance).asInstanceOf[AnyRef]

    val globalClass = classLoader.loadClass("scala.tools.nsc.Global")
    val abstractReporterClass = classLoader.loadClass("scala.tools.nsc.reporters.Reporter")
    val globalContructor = globalClass.getConstructor(settingsClass, abstractReporterClass)
    val globalInstance = globalContructor.newInstance(settingsInstance, reporterInstance).asInstanceOf[AnyRef]

    val runClass = globalInstance.getClass().getClasses.find { c =>
      c.getName.contains("Run")
    } match {
      case Some(runClass) => runClass
      case None => throw new RuntimeException("No Run class found")
    }
    val runInstance = runClass.getConstructor(globalClass).newInstance(globalInstance).asInstanceOf[AnyRef]
    val compileMethod = runClass.getMethod("compileSources", classLoader.loadClass("scala.collection.immutable.List"))
    val sources = prepareSources(sourceDir)
    val runArgs = Array(sources.toList)
    compileMethod.invoke(runInstance, runArgs: _*)
  }

  def prepareSources(sourceDir: String) = scanSourceFilesInDir(sourceDir) map { fileName =>
    println("Compiling file: " + fileName)
    val fileContents = Source.fromFile(fileName).getLines().mkString("\n")
    new BatchSourceFile("<source>", fileContents)
  }

  def scanSourceFilesInDir(sourceDir: String): List[String] = {
    @tailrec
    def scanSourceFilesInDir(addedFiles: List[String], nonProcessedDirs: List[String]): List[String] = {
      if (nonProcessedDirs.isEmpty) {
        addedFiles
      } else {
        val sourceDir = nonProcessedDirs.head
        val srcDirEntries = new File(sourceDir).list().toList map (sourceDir + File.separator + _)
        val (srcDirDirs, srcDirFiles) = srcDirEntries.partition ( new File(_).isDirectory )
        val scalaFiles = srcDirFiles collect { case srcFile if srcFile.endsWith(".scala") => srcFile }
        scanSourceFilesInDir(addedFiles ++ scalaFiles, nonProcessedDirs.tail ++ srcDirDirs)
      }
    }
    scanSourceFilesInDir(Nil, List(sourceDir))
  }
}
