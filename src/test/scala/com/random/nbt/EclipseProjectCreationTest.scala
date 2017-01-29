package com.random.nbt

import java.io.File

import org.scalatest._
import scala.io.Source

class EclipseProjectCreationTest extends FlatSpec with MustMatchers with BeforeAndAfter {
  implicit val context = new Context
  val projectName = "Test"
  val workingDir = s"/tmp/$projectName"

  "InternalCallHandler" should "create Eclipse project" in {
    implicit val context = new Context
    implicit val ivyManager = new IvyManager
    context.set("projectName", projectName)
    context.set("dependenciesAsJarPaths", "scalaz-core.jar")
    context.set("currentDir", workingDir)
    CommandLineExecutor.execute(s"mkdir $workingDir")
    new InternalCallHandler("createEclipseProject").apply()
    contains(s"$workingDir/.project", "<name>Test</name>") mustBe true
    contains(s"$workingDir/.classpath", "scalaz-core.jar") mustBe true
    new File(s"$workingDir/src").exists() mustBe true
    new File(s"$workingDir/bin").exists() mustBe true
  }

  private def contains(fileName: String, text: String) = {
    val fileContents = Source.fromFile(fileName).mkString
    fileContents.contains(text)
  }

  before {
    CommandLineExecutor.execute(s"rm -rf $workingDir")
  }

  after {
    CommandLineExecutor.execute(s"rm -rf $workingDir")
  }
}
