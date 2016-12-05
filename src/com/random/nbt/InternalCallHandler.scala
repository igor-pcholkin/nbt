package com.random.nbt

import scala.util.Failure
import scala.util.Success
import scala.util.Try

object InternalCallHandler {
  val ivyHelper = new IvyHelper()
}

class InternalCallHandler(methodName: String, callParams: Array[String]) extends FileUtils {
  import InternalCallHandler._

  def compile() = {
    val currentDir = Context.getString("projectDir", "currentDir").getOrElse(".")
    val sourceDir = if (callParams.length > 0) callParams(0) else createAbsolutePath(currentDir, "src")
    val destDir = if (callParams.length > 1) callParams(1) else createAbsolutePath(currentDir, "bin")
    println("Compile source dir: " + sourceDir)
    new ScalaCompiler().compile(sourceDir, destDir)
  }

  def findMainClass(): Unit = {
    val binDir = callParams(0)
    println("Finding main class in bin dir: " + binDir)
    Context.set("mainClass", findMainClass(binDir).getOrElse(""))
  }

  def findScalaLibrary(): Unit = {
    val mayBeScalaVersion = Context.getString("scalaVersion").
      orElse(ivyHelper.getLastLocalVersion("org.scala-lang", "scala-library"))
    mayBeScalaVersion match {
      case Some(scalaVersion) =>
        val scalaLibPath = ivyHelper.getModuleJarFileName("org.scala-lang", "scala-library", scalaVersion)
        Context.set("scalaLibrary", scalaLibPath)
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

  def resolveDependenciesAsJarPaths() = {
    val jarPaths = (Context.get("dependencies") match {
      case Some(compileDependencies) => parseDependencies(compileDependencies.asInstanceOf[Array[String]]) flatMap { case (org, module) =>
        ivyHelper.getLastLocalVersionFilePath(org, module) match {
          case jar@Some(s:String) => jar
          case None => resolveJarPathUsingScalaMajorMinorVersion(org, module)
        }
      }
      case None => Nil
    }) mkString(":")
    println("Setting dependenciesAsJarPaths: " + jarPaths)
    Context.set("dependenciesAsJarPaths", jarPaths)
  }

  def resolveJarPathUsingScalaMajorMinorVersion(org: String, module: String) = {
    getScalaMajorMinorVersion flatMap { scalaMajorMinorVersion =>
      println(s"Not able to resolve dependency: $org:$module, trying $org:${module}_${scalaMajorMinorVersion}")
      ivyHelper.getLastLocalVersionFilePath(org, s"${module}_${scalaMajorMinorVersion}")
    }
  }

  def getScalaMajorMinorVersion = {
    Context.getString("scalaVersion") map { scalaVersion =>
      val parts = scalaVersion.split("\\.")
      s"${parts(0)}.${parts(1)}"
    }
  }

  private def parseDependencies(rawDependencies: Seq[String]) = {
    rawDependencies flatMap { d =>
      val dParts = d.split(":")
      if (dParts.length < 2) {
        println(s"Error: can't parse dependency $d")
        None
      } else {
        Some((dParts(0), dParts(1)))
      }
    }
  }

  def setVar() = {
    val rawAssignment = callParams.mkString
    val assignment = rawAssignment.split("=")
    if (assignment.length == 2) {
      val (variable, value) = (assignment(0).trim, assignment(1).trim)
      println(s"Setting var: $variable = $value")
      if (value.contains(",")) {
        Context.set(variable, value.split("[,\\s]+"))
      } else {
        Context.set(variable, value)
      }
    } else {
      println(s"Invalid assignment: $rawAssignment")
    }
  }

  def handle() = {
    Try {
      getClass.getMethod(methodName)
    } match {
      case Success(method) => method.invoke(this)
      case Failure(ex)     => println(s"Error when resolving $methodName: ${ex.getMessage}")
    }
  }

}