package com.random.nbt

import scala.collection.mutable.Map
import scala.util.Try
import scala.util.Failure
import scala.util.Success

object InternalCallHandler {
  val ivyHelper = new IvyHelper()
}

class InternalCallHandler(methodName: String, callParams: Array[String])(implicit val context: Map[String, String]) extends FileUtils {
  import InternalCallHandler._

  def compile() = {
    val sourceDir = callParams(0)
    println("Compile source dir: " + sourceDir)
    new ScalaCompiler().compile(sourceDir)
  }

  def findMainClass(): Unit = {
    val binDir = callParams(0)
    println("Finding main class in bin dir: " + binDir)
    context += ("mainClass" -> findMainClass(binDir).getOrElse(""))
  }

  def findScalaLibrary(): Unit = {
    val mayBeScalaVersion = context.get("scalaVersion").orElse(ivyHelper.getLastLocalVersion("org.scala-lang", "scala-library"))
    mayBeScalaVersion match {
      case Some(scalaVersion) =>
        val scalaLibPath = ivyHelper.getModuleJarFileName("org.scala-lang", "scala-library", scalaVersion)
        context += ("scalaLibrary" -> scalaLibPath)
      case None => println("Error: no scala library is found")
    }
  }

  def listLocalRevisions(): Unit = {
    val org = callParams(0)
    val module = callParams(1)
    println(s"Searching ivy modules: $org $module")
    ivyHelper.getLocalModuleVersions(org, module) foreach (println(_))
  }

  def getAvailableModuleVersions(): Unit = {
    val org = callParams(0)
    val module = callParams(1)
    println(s"Searching versions for artifact: $org $module")
    ivyHelper.getAvailableModuleVersions(org, module) foreach (println(_))
  }

  def resolveModuleVersion(): Unit = {
    val org = callParams(0)
    val module = callParams(1)
    val version = callParams(2)
    println(s"Resolving version for artifact: $org $module $version")
    ivyHelper.resolveModule(org, module, version)
  }

  def getModuleJarFileName(): Unit = {
    val org = callParams(0)
    val module = callParams(1)
    val version = callParams(2)
    println("Module file name: " + ivyHelper.getModuleJarFileName(org, module, version))
  }

  def setScalaVersion(): Unit = {
    val scalaVersion = callParams(0)
    println(s"Setting scala version to: $scalaVersion")
    context += ("scalaVersion" -> scalaVersion)
  }

  def handle() = {
    Try {
      getClass.getMethod(methodName)
    } match {
      case Success(method) => method.invoke(this)
      case Failure(ex)     => println(s"Error when calling $methodName: ${ex.getMessage}")
    }
  }

}
