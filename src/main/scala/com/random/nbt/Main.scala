package com.random.nbt

import java.io.File
import com.typesafe.scalalogging.LazyLogging
import java.io.StringWriter
import java.io.PrintWriter
import scala.io.Source
import java.io.FileNotFoundException
import scala.util.Try
import scala.util.Failure
import scala.util.Success

object Main extends App with FileUtils with LazyLogging {
  implicit val context = new Context

  if (args.length > 0) {
    if (args(0) == "create")
      createDependenciesFile(args.slice(1, args.length).mkString("").split(","))
    else {
      val (phaseName, vars) = parseArgs(args)
      addVarsToContext(vars)
      runConfiguredPhase(phaseName)
    }
  } else {
    println("Usage: nbt <build phase>| create")
  }

  def addVarsToContext(vars: Map[String, String]) = {
    vars.foreach { case (varName, value) =>
      context.setRaw(varName, value)
    }
  }

  def runConfiguredPhase(phaseName: String) = {
    implicit val phases = new ConfigParser().parse()
    setDependencies()
    readShortcuts()
    val phaseExecutor = new PhaseExecutor()
    phaseExecutor.runPhase("init")
    phaseExecutor.runPhase(phaseName)
  }

  def parseArgs(args: Array[String]): (String, Map[String, String]) = {
    if (args(0).startsWith("-")) {
      (args.last, parseSwitches(args.slice(0, args.length - 1)))
    } else {
      (args(0), parseSwitches(args.slice(1, args.length)))
    }
  }

  def parseSwitches(switches: Array[String]) = {
    val emptyMap = Map[String, String]()
    if (switches.isEmpty)
      emptyMap
    else {
      if (switches(0) == "-vars") {
        val paramList = switches.slice(1, switches.length)
        (paramList map { param =>
          val parts = param.split("=")
          (parts(0), parts(1))
        }).toMap
      } else {
        emptyMap
      }
    }
  }

  def createDependenciesFile(dependencies: Array[String]) = {
    val pw = new PrintWriter(new File("dependencies"))
    dependencies map { dependency =>
      pw.println(dependency)
    }
    pw.close()
  }

  def setDependencies() = {
    Try {
      val dependencies = Source.fromFile("dependencies").getLines().toSeq
      logger.info(s"Reading dependencies from dependencies")
      context.set(Context.DEPENDENCIES, dependencies)
    } match {
      case Failure(ex:FileNotFoundException) => logger.info("No dependencies file is found")
      case Success(_) =>
      case err@_ => logger.error(s"Error reading project dependencies file")
    }
  }

  def readShortcuts() = {
    Try {
      val shortcutsLines = Source.fromInputStream(getClass().getResourceAsStream("/shortcuts.conf")).getLines().toArray
      val shortcuts = (shortcutsLines map { shortcutLine =>
        val sParts = shortcutLine.split("->") map (_.trim)
        sParts(0) -> sParts(1)
      }).toMap
      logger.info(s"Reading shortcuts from shortcuts.def")
      context.set("shortcuts", shortcuts)
    } match {
      case Failure(ex:FileNotFoundException) =>
      case Success(_) =>
      case err@_ => logger.error(s"Error reading shortcuts file")
    }
  }
}
