package com.random.nbt

import java.io.File

import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.io.Source
import com.typesafe.scalalogging.LazyLogging

object InternalCallHandler {
  val ivyHelper = new IvyHelper()
  def getProjectDir = Context.getString("projectDir", "currentDir")
}

class InternalCallHandler(methodName: String, callParams: Array[String]) extends FileUtils with LazyLogging {
  import InternalCallHandler._

  def compile() = {
    val currentDir = getProjectDir.getOrElse(".")
    val sourceDir = if (callParams.length > 0) callParams(0) else createAbsolutePath(currentDir, "src")
    val destDir = if (callParams.length > 1) callParams(1) else createAbsolutePath(currentDir, "bin")
    logger.info("Compile source dir: " + sourceDir)
    new ScalaCompiler().compile(sourceDir, destDir)
  }

  def findMainClass(): Boolean = {
    val binDir = callParams(0)
    logger.info("Finding main class in bin dir: " + binDir)
    Context.set("mainClass", findMainClass(binDir).getOrElse(""))
    true
  }

  def findScalaLibrary(): Boolean = {
    val mayBeScalaVersion = Context.getString("scalaVersion").
      orElse(ivyHelper.getLastLocalVersion("org.scala-lang", "scala-library"))
    mayBeScalaVersion match {
      case Some(scalaVersion) =>
        val scalaLibPath = ivyHelper.getModuleJarFileName("org.scala-lang", "scala-library", scalaVersion)
        Context.set("scalaLibrary", scalaLibPath)
        true
      case None =>
        logger.error("No scala library is found")
        false
    }
  }

  def listLocalRevisions(): Boolean = {
    val org = callParams(0)
    val module = callParams(1)
    logger.info(s"Searching ivy modules: $org $module")
    ivyHelper.getLocalModuleVersions(org, module) foreach (println(_))
    true
  }

  def getAvailableModuleVersions(): Boolean = {
    val org = callParams(0)
    val module = callParams(1)
    logger.info(s"Searching versions for artifact: $org $module")
    ivyHelper.getAvailableModuleVersions(org, module) foreach (println(_))
    true
  }

  def resolveModuleVersion(): Boolean = {
    val org = callParams(0)
    val module = callParams(1)
    val version = callParams(2)
    logger.info(s"Resolving version for artifact: $org $module $version")
    ivyHelper.resolveModule(org, module, version)
    true
  }

  def getModuleDependenciesInfo(): Boolean = {
    val org = callParams(0)
    val module = callParams(1)
    val version = callParams(2)
    val depConfiguration = callParams(3)
    logger.info(s"Resolving version for artifact: $org $module $version $depConfiguration")
    ivyHelper.getModuleDependenciesInfo(org, module, version, depConfiguration) foreach (println(_))
    true
  }

  def getModuleJarFileName(): Boolean = {
    val org = callParams(0)
    val module = callParams(1)
    val version = callParams(2)
    println("Module file name: " + ivyHelper.getModuleJarFileName(org, module, version))
    true
  }

  def resolveDependenciesAsJarPaths(): Boolean = {
    val jarPaths = (Context.get("dependencies") match {
      case Some(compileDependencies) => parseDependencies(compileDependencies.asInstanceOf[Seq[String]]) flatMap { case (org, module) =>
        ivyHelper.getLastLocalVersionFilePath(org, module) match {
          case jar@Some(s:String) => jar
          case None => Nil
        }
      }
      case None => Nil
    }) mkString(":")
    logger.info("Setting dependenciesAsJarPaths: " + jarPaths)
    Context.set("dependenciesAsJarPaths", jarPaths)
    true
  }

  private def parseDependencies(rawDependencies: Seq[String]) = {
    rawDependencies flatMap { d =>
      val dParts = d.split(":")
      if (dParts.length < 2) {
        logger.error(s"Can't parse dependency $d")
        None
      } else {
        Some((dParts(0), dParts(1)))
      }
    }
  }

  def setVar(): Boolean = {
    val rawAssignment = callParams.mkString
    val assignment = rawAssignment.split("=")
    if (assignment.length == 2) {
      val (variable, value) = (assignment(0).trim, assignment(1).trim)
      logger.info(s"Setting var: $variable = $value")
      val value2Set = if (value.contains(",")) value.split("[,\\s]+") else value
      Context.set(variable, value2Set)
      true
    } else {
      logger.error(s"Invalid assignment: $rawAssignment")
      false
    }
  }

  def recencyCheck(): Boolean = {
    val srcDir = callParams(0)
    val binDir = callParams(1)
    val (updatedSrcFiles, binFiles) = getProjectDir match {
      case Some(projectDir) =>
        val updatedSrcFiles = RecencyCache.getUpdatedAndDependingSrcFiles(srcDir)
        (updatedSrcFiles, RecencyCache.getCachedBinFileEntries)
      case None =>
        logger.error("no project dir set")
        (getAllSourceFiles(srcDir), Nil)
    }
    Context.set("updatedSrcFiles-" + srcDir, updatedSrcFiles)
    Context.set("cachedBinFiles-" + binDir, binFiles)
    logger.info(s"Setting recent files: ${updatedSrcFiles.mkString(",")}")
    true
  }

  def updateCache(): Boolean = {
    val srcDir = callParams(0)
    val binDir = callParams(1)
    getProjectDir match {
      case Some(projectDir) =>
        val allSrcFiles = getAllSourceFiles(srcDir)
        val allBinDirFiles = getAllDirFiles(binDir)
        val srcDependencies = getAllSrcDependencies(srcDir)
        logger.info(s"Determine source dependencies: $srcDependencies")
        RecencyCache.refresh(allSrcFiles, allBinDirFiles, srcDependencies)
        true
      case None =>
        logger.error("no project dir set")
        false
    }
  }

  private def getAllSrcDependencies(srcDir: String) = {
    val srcFiles = getAllSourceFiles(srcDir)
    val importsGroupedByFile = (srcFiles map { srcFile =>
      (srcFile, getImports(srcFile))
    })
    scala.collection.mutable.Map[String, Seq[String]]() ++ (( importsGroupedByFile flatMap { case (depending, imports) =>
      imports map { imp => (getSourceFileNameFromClassName(imp, srcDir), depending) }
    }) groupBy (_._1) map { e => e._1 -> (e._2 map { kv => kv._2 }) })
  }

  private def getImports(srcFile: String) = {
    Source.fromFile(srcFile).getLines() flatMap { line =>
      val importPattern = "import ([^\\n]*)".r
      line match {
        case importPattern(dependency) => Some(dependency)
        case _ => None
      }
    }
  }

  def handle(): Boolean = {
    Try {
      getClass.getMethod(methodName)
    } match {
      case Success(method) => method.invoke(this).asInstanceOf[Boolean]
      case Failure(ex)     =>
        logger.error(s"Error when resolving $methodName: ${ex.getMessage}")
        false
    }
  }

}
