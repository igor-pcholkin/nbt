package com.random.nbt

import scala.collection.mutable.Map

class PhaseExecutor extends FileUtils {
  def runPhase(phaseName: String)(implicit phases: List[Phase], context: Map[String, String]) = {
    phases.find(_.name == phaseName) match {
      case Some(phase) => execute(phase)
      case None => println(s"Error: No such phase found: ${phaseName}")
    }
  }

  def executeDependantPhases(phase: Phase)(implicit phases: List[Phase], context: Map[String, String]) = {
    phase.dependsOn map { dependantPhase =>
      phases.find(_.name == dependantPhase.name) match {
        case Some(resolvedPhase) => execute(resolvedPhase)
        case None => println(s"Error: No dependant phase found: ${dependantPhase.name}")
      }
    }
  }

  def execute(phase: Phase)(implicit phases: List[Phase], context: Map[String, String]): Unit = {
    executeDependantPhases(phase)
    executeInternalCalls(phase)
    executeCommmandsOfPhase(phase)
  }

  def executeInternalCalls(phase: Phase)(implicit context: Map[String, String]) = {
    if (phase.calls.nonEmpty) {
      val callLine = resolveVarsInCommmandLine(phase.calls.head)
      val callParams = callLine.split("[ \t]+")
      callParams(0) match {
        case "compile" =>
          val sourceDir = callParams(1)
          println("Compile source dir: " + sourceDir)
          new ScalaCompiler().compile(sourceDir)
        case "findMainClass" =>
          val binDir = callParams(1)
          println("Finding main class in bin dir: " + binDir)
          context += ("mainClass" -> findMainClass(binDir).getOrElse(""))
        case "findScalaLibrary" =>
          context += ("scalaLibrary" -> "/Users/igor/Downloads/scala-2.11.8/lib/scala-library.jar")
        case "listLocalRevisions" =>
          val org = callParams(1)
          val module = callParams(2)
          println(s"Searching ivy modules: $org $module")
          new IvyHelper().getLocalModuleVersions(org, module) foreach (println(_))
        case "getAvailableModuleVersions" =>
          val org = callParams(1)
          val module = callParams(2)
          println(s"Searching versions for artifact: $org $module")
          new IvyHelper().getAvailableModuleVersions(org, module) foreach (println(_))
        case "resolveModuleVersion" =>
          val org = callParams(1)
          val module = callParams(2)
          val version = callParams(3)
          println(s"Resolving version for artifact: $org $module $version")
          new IvyHelper().resolveModule(org, module, version)
        case "getModuleJarFileName" =>
          val org = callParams(1)
          val module = callParams(2)
          val version = callParams(3)
          println("Module file name: " + new IvyHelper().getModuleJarFileName(org, module, version))
      }
    }
  }

  def executeCommmandsOfPhase(phase: Phase)(implicit context: Map[String, String]) = {
    phase.cmdLines map { cmd =>
      executeCmdLine(cmd)
    }
  }

  def executeCmdLine(cmdLine: String)(implicit context: Map[String, String]) = {
    import sys.process._
    val refinedCmdLine = resolveVarsInCommmandLine(cmdLine)
    println(s"Executing command: $refinedCmdLine")
    refinedCmdLine.!
  }

  def resolveVarsInCommmandLine(cmdLine: String)(implicit context: Map[String, String]) = {
    context.keys.foldLeft(cmdLine) { (cmdLine, key) =>
      cmdLine.replace("$" + s"$key", s"${context.getOrElse(key, "")}")
    }
  }

}
