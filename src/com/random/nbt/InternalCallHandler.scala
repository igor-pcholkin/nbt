package com.random.nbt

import java.io.File

import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.io.Source

object InternalCallHandler {
  val ivyHelper = new IvyHelper()
  def getProjectDir = Context.getString("projectDir", "currentDir")
}

class InternalCallHandler(methodName: String, callParams: Array[String]) extends FileUtils {
  import InternalCallHandler._

  def compile() = {
    val currentDir = getProjectDir.getOrElse(".")
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
      case Some(compileDependencies) => parseDependencies(compileDependencies.asInstanceOf[Seq[String]]) flatMap { case (org, module) =>
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

  def recencyCheck() = {
    val srcDir = callParams(0)
    val binDir = callParams(1)
    val (updatedSrcFiles, binFiles) = getProjectDir match {
      case Some(projectDir) =>
        val updatedSrcFiles = RecencyCache.getUpdatedAndDependingSrcFiles(srcDir)
        (updatedSrcFiles, RecencyCache.getCachedBinFileEntries)
      case None =>
        println("Error: no project dir set")
        (getAllSourceFiles(srcDir), Nil)
    }
    Context.set("updatedSrcFiles-" + srcDir, updatedSrcFiles)
    Context.set("cachedBinFiles-" + binDir, binFiles)
    println(s"Setting recent files: ${updatedSrcFiles.mkString(",")}")
  }

  def updateCacheWithBinEntriesAndSourceDependencies() = {
    val srcDir = callParams(0)
    val binDir = callParams(1)
    val recentFiles =  getProjectDir match {
      case Some(projectDir) =>
        val allBinDirFiles = getAllDirFiles(binDir)
        val srcDependencies = getAllSrcDependencies(srcDir)
        println(s"Determine source dependencies: $srcDependencies")
        RecencyCache.refresh(Some(RecencyCache.getCachedSrcFileEntries), Some(allBinDirFiles), Some(srcDependencies))
      case None => println("Error: no project dir set")
    }
  }

  private def getAllSrcDependencies(srcDir: String) = {
    val srcFiles = getAllSourceFiles(srcDir)
    val dependingToDependencies = (srcFiles map { srcFile =>
      (srcFile , getImports(srcFile))
    })
    scala.collection.mutable.Map[String, Seq[String]]() ++ (( dependingToDependencies flatMap { case (depending, imports) =>
      imports map { imp => (getSourceFileNameFromClassName(imp, srcDir), depending) }
    }) groupBy (_._1) map { e => e._1 -> (e._2 map { kv => kv._2 }) })
  }

  def getImports(srcFile: String) = {
    Source.fromFile(srcFile).getLines() flatMap { line =>
      val importPattern = "import ([^\\n]*)".r
      line match {
        case importPattern(dependency) => Some(dependency)
        case _ => None
      }
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
