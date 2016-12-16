package com.random.nbt

import java.io.File
import com.typesafe.scalalogging.LazyLogging
import java.io.StringWriter
import java.io.PrintWriter
import scala.io.Source

object Main extends App with FileUtils with LazyLogging {
  if (args.length > 0) {
    if (args(0) == "create")
      createDependenciesFile(args.slice(1, args.length).mkString("").split(","))
    else {
      implicit val phases = new ConfigParser().parse()
      setDependencies()
      val phaseExecutor = new PhaseExecutor()
      phaseExecutor.runPhase("init")
      phaseExecutor.runPhase(args(0))
    }
  } else {
    println("Usage: nbt <build phase>| create")
  }

  def createDependenciesFile(dependencies: Array[String]) = {
    val pw = new PrintWriter(new File("dependencies"))
    dependencies map { dependency =>
      pw.println(dependency)
    }
    pw.close()
  }

  def setDependencies() = {
    val dependencies = Source.fromFile("dependencies").getLines().toArray
    logger.info(s"Reading dependencies from dependencies")
    Context.set("dependencies", dependencies)
  }
}
