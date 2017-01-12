package com.random.nbt

import java.io.File

import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.io.Source
import com.typesafe.scalalogging.LazyLogging
import Util._
import Types._

class InternalCallHandler(methodName: String, callParams: Array[String] = Array())(implicit context: Context) extends FileUtils with LazyLogging {
  val ivyManager = new IvyManager()
  def getProjectDir = context.getString("projectDir", "currentDir")

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
    val mainClass = findMainClass(binDir) match {
      case scalaz.Success(mainClass) => mainClass
      case scalaz.Failure(err) =>
        logger.error(err)
        ""
    }
    context.set("mainClass", mainClass)
    true
  }

  def findScalaLibrary(): Boolean = {
    val mayBeScalaVersion = context.getString("scalaVersion").
      orElse(ivyManager.getLastLocalVersionIgnoreModuleName("org.scala-lang", "scala-library"))
    mayBeScalaVersion match {
      case Some(scalaVersion) =>
        val scalaLibPath = ivyManager.getModuleJarFileName("org.scala-lang", "scala-library", scalaVersion)
        context.set("scalaLibrary", scalaLibPath)
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
    val (correctedModule, versions) = ivyManager.getLocalModuleVersions(org, module)
    println("Module: $correctedModule")
    versions foreach (println(_))
    true
  }

  def getAvailableModuleVersions(): Boolean = {
    val org = callParams(0)
    val module = callParams(1)
    logger.info(s"Searching versions for artifact: $org $module")
    val (correctedModule, versions) = ivyManager.getAvailableModuleVersions(org, module)
    versions foreach (println(_))
    true
  }

  def resolveModuleVersion(): Boolean = {
    val org = callParams(0)
    val module = callParams(1)
    val version = callParams(2)
    val depConfig = callParams(3)
    logger.info(s"Resolving version for artifact: $org $module $version $depConfig")
    ivyManager.resolveModule(org, module, version, "default", "master")
    ivyManager.resolveModule(org, module, version, "default", depConfig)
    true
  }

  def resolveModuleSources(): Boolean = {
    val org = callParams(0)
    val module = callParams(1)
    val version = callParams(2)
    logger.info(s"Resolving version for artifact: $org $module $version")
    ivyManager.resolveModuleSources(org, module, version)
    true
  }

  def getModuleDependenciesInfo(): Boolean = {
    val org = callParams(0)
    val module = callParams(1)
    val version = callParams(2)
    val depConfiguration = callParams(3)
    logger.info(s"Resolving version for artifact: $org $module $version $depConfiguration")
    ivyManager.getModuleDependenciesInfo(org, module, version, depConfiguration, false) foreach (println(_))
    true
  }

  def getModuleJarFileName(): Boolean = {
    val org = callParams(0)
    val module = callParams(1)
    val version = callParams(2)
    println("Module file name: " + ivyManager.getModuleJarFileName(org, module, version))
    true
  }

  def resolveDependenciesAsJarPaths(): Boolean = {
    val configuration = callParams(0)
    val jarPaths = (context.get(Context.DEPENDENCIES) match {
      case SomeSeqString(mayBeDependencies) => getDependenciesJarsTransitive(mayBeDependencies.getOrElse(Nil), configuration)
      case pp@_ => Nil
    }) mkString(":")
    logger.info("Setting dependenciesAsJarPaths: " + jarPaths)
    context.set("dependenciesAsJarPaths", jarPaths)
    true
  }

  private def getDependenciesJarsTransitive(dependencies: Seq[String], configuration: String) = {
    parseDependencies(dependencies) flatMap { case (org, module) =>
        getDependencyJarsTransitive(org, module, configuration)
      }
  }

  private def getDependencyJarsTransitive(org: String, module: String, configuration: String) = {
    val (correctedModule, mayBeRevision) = ivyManager.getLastLocalVersion(org, module)
    mayBeRevision match {
      case Some(revision) =>
        val allDependencies = ivyManager.getModuleDependenciesInfo(org, module, revision, configuration, true)
        allDependencies flatMap { ivyNode =>
          val moduleId = ivyNode.getId.getModuleId
          ivyManager.getLastLocalVersionFilePath(moduleId.getOrganisation, moduleId.getName)
        }
      case None => Array[String]()
    }
  }

  private def parseDependencies(rawDependencies: Seq[String]) = {
    rawDependencies flatMap { d =>
      parseDependency(d)
    }
  }

  private def parseDependency(d: String) = {
    if (d.contains(":"))
      parseDependencyAsOrModule(d)
    else
      parseDependencyAsShortcut(d)
  }

  private def parseDependencyAsShortcut(sKey: String) = {
    context.get("shortcuts") match {
      case SomeMapString(rawShortcuts) =>
        val shortcuts = rawShortcuts.getOrElse(Map.empty)
        shortcuts.get(sKey) match {
          case Some(d) => parseDependencyAsOrModule(d)
          case None =>
            logger.error(s"Invalid shortcut: $sKey")
            None
        }
      case None =>
        logger.error(s"No shortcuts defined for $sKey")
        None
    }
  }

  private def parseDependencyAsOrModule(d: String) = {
    val dParts = d.split(":")
    if (dParts.length < 2) {
      logger.error(s"Can't parse dependency $d")
      None
    } else {
      Some((dParts(0), dParts(1)))
    }
  }

  def setVar(): Boolean = {
    val rawAssignment = callParams.mkString
    val assignment = rawAssignment.split("=")
    if (assignment.length == 2) {
      val (varName, value) = (assignment(0).trim, assignment(1).trim)
      context.setRaw(varName, value)
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
        val recencyCache = new RecencyCache()
        val updatedSrcFiles = recencyCache.getUpdatedAndDependingSrcFiles(srcDir)
        (updatedSrcFiles, recencyCache.getCachedBinFileEntries)
      case None =>
        logger.error("no project dir set")
        (getAllSourceFiles(srcDir), Nil)
    }
    context.set("updatedSrcFiles-" + srcDir, updatedSrcFiles)
    context.set("cachedBinFiles-" + binDir, binFiles)
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
        new RecencyCache().refresh(allSrcFiles, allBinDirFiles, srcDependencies)
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
    scala.collection.mutable.Map[String, Seq[String]]() ++ grouped( importsGroupedByFile flatMap { case (depending, imports) =>
      imports map { imp => (getSourceFileNameFromClassName(imp, srcDir), depending) }
    })
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

  def createEclipseProject() = {
    new EclipseManager().createProject()
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
