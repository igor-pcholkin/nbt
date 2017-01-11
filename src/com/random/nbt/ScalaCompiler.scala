package com.random.nbt

import java.io.File
import scala.reflect.internal.util.BatchSourceFile
import scala.io.Source
import scala.annotation.tailrec
import java.net.URL
import scala.collection.mutable.Map
import com.typesafe.scalalogging.LazyLogging
import Types._

object ScalaCompiler {
  val org = "org.scala-lang"
  val scalaCompiler = "scala-compiler"
  val scalaReflect = "scala-reflect"
}

class ScalaCompiler(implicit context: Context) extends FileUtils with LazyLogging {
  import ScalaCompiler._

  val ivyManager = new IvyManager()

  def compile(sourceDir: String, destDir: String) = {
    val scalaVersion = context.getString("scalaVersion")
    val mayBeScalaCompilerVersion = scalaVersion.orElse(ivyManager.getLastLocalVersionIgnoreModuleName(org, scalaCompiler))
    val mayBeScalaReflectVersion = scalaVersion.orElse(ivyManager.getLastLocalVersionIgnoreModuleName(org, scalaReflect))
    (mayBeScalaCompilerVersion, mayBeScalaReflectVersion) match {
      case (Some(scalaCompilerVersion), Some(scalaReflectVersion)) =>
        if (scalaCompilerVersion == scalaReflectVersion) {
          compileUsing(sourceDir, destDir, scalaCompilerVersion)
        } else {
          logger.error(s"Error: latest scala-compiler ($scalaCompilerVersion) and scala-reflect ($scalaReflectVersion) version mismatch")
          false
        }
      case _ =>
        logger.error("Error: No scala compiler or dependant libs found")
        false
    }
  }

  def compileUsing(sourceDir: String, destDir: String, scalaVersion: String): Boolean = {
    val scalaCompilerJarPath = ivyManager.getModuleJarFileName(org, scalaCompiler, scalaVersion)
    val scalaReflectJarPath = ivyManager.getModuleJarFileName(org, scalaReflect, scalaVersion)
    val classPath = List(scalaCompilerJarPath, scalaReflectJarPath) map { jarPath =>
      new URL(s"file://$jarPath")
    }
    implicit val classLoader = new java.net.URLClassLoader(classPath.toArray)
    logger.info(s"""Compile using classpath: ${classPath.mkString(":")}""")
    compileUsing(sourceDir, destDir)
  }

  def compileUsing(sourceDir: String, destDir: String) (implicit classLoader: ClassLoader) = {

    val settingsClass = classLoader.loadClass("scala.tools.nsc.Settings")
    val settingsInstance = settingsClass.getConstructor().newInstance().asInstanceOf[AnyRef]

    addCompileArguments(destDir, settingsClass, settingsInstance)

    val reporterClass = classLoader.loadClass("scala.tools.nsc.reporters.ConsoleReporter")
    val reporterInstance = reporterClass.getConstructor(settingsClass).newInstance(settingsInstance).asInstanceOf[AnyRef]

    val globalClass = classLoader.loadClass("scala.tools.nsc.Global")
    val abstractReporterClass = classLoader.loadClass("scala.tools.nsc.reporters.Reporter")
    val globalContructor = globalClass.getConstructor(settingsClass, abstractReporterClass)
    val globalInstance = globalContructor.newInstance(settingsInstance, reporterInstance).asInstanceOf[AnyRef]

    invokeCompileMethod(sourceDir, destDir: String, globalClass, globalInstance)

    returnCompileResult(reporterInstance, reporterClass)
  }

  def returnCompileResult(reporterInstance: AnyRef, reporterClass: Class[_])(implicit classLoader: ClassLoader) = {
    val hasErrorsMethod = reporterClass.getMethod("hasErrors")
    val hasErrors = hasErrorsMethod.invoke(reporterInstance).asInstanceOf[Boolean]
    !hasErrors
  }

  def addCompileArguments(destDir: String, settingsClass: Class[_], settingsInstance: AnyRef)
    (implicit classLoader: ClassLoader) = {
    val classPath = context.get("dependenciesAsJarPaths").getOrElse("")
    val cpArgs = s"""-classpath "$classPath:$destDir" -d $destDir"""
    logger.info(s"Passing additional dependencies for compiler: $cpArgs")
    val processArgumentStringMethod = settingsClass.getMethod("processArgumentString", classLoader.loadClass("java.lang.String"))
    processArgumentStringMethod.invoke(settingsInstance, cpArgs)
  }

  def invokeCompileMethod(sourceDir: String, binDir: String, globalClass: Class[_], globalInstance: AnyRef)(
      implicit classLoader: ClassLoader) = {
    val runClass = globalInstance.getClass().getClasses.find { c =>
      c.getName.contains("Run")
    } match {
      case Some(runClass) => runClass
      case None           => throw new RuntimeException("No Run class found")
    }
    val runInstance = runClass.getConstructor(globalClass).newInstance(globalInstance).asInstanceOf[AnyRef]
    val compileMethod = runClass.getMethod("compileSources", classLoader.loadClass("scala.collection.immutable.List"))
    val sources = prepareSources(sourceDir, binDir)
    if (sources.length > 0) {
      val runArgs = Array(sources.toList)
      compileMethod.invoke(runInstance, runArgs: _*)
    }
  }

  def prepareSources(srcDir: String, binDir: String) = {
    findFilesToCompile(srcDir, binDir) map { fileName =>
      logger.info("Compiling file: " + fileName)
      val fileContents = Source.fromFile(fileName).getLines().mkString("\n")
      new BatchSourceFile(fileName, fileContents)
    }
  }

  def findFilesToCompile(srcDir: String, binDir: String) = {
    if (isMissingAnyBinaryFile(binDir)) {
      logger.info("Some binaries are missing: (re)compiling everything")
      getAllSourceFiles(srcDir)
    } else {
      context.get("updatedSrcFiles-" + srcDir) match {
        case SomeSeqString(mayBeFiles) => mayBeFiles.getOrElse(Nil)
        case _                   => getAllSourceFiles(srcDir)
      }
    }
  }

  def isMissingAnyBinaryFile(binDir: String) = {
    context.get("cachedBinFiles-" + binDir) match {
      case SomeSeqString(mayBeFiles) =>
        val cachedBinFiles = mayBeFiles.getOrElse(Nil)
        cachedBinFiles.exists(!new File(_).exists())
      case _ => true
    }
  }

}
