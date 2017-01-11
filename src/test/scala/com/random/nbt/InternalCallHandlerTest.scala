package com.random.nbt

import java.io.File

import org.scalatest._

class InternalCallHandlerTest extends FlatSpec with MustMatchers with BeforeAndAfter {
  val projectName = "Test"
  val workingDir = s"/tmp/$projectName"

  "InternalCallHandler" should "create Eclipse project" in {
    implicit val context = new Context
    context.set("project", projectName)
    context.set("dependencies", "scalaz-core.jar")
    context.set("currentDir", workingDir)
    CommandLineExecutor.execute(s"mkdir $workingDir")
    new InternalCallHandler("createEclipseProject").handle()
    new File(s"$workingDir/.project").exists() mustBe true
    new File(s"$workingDir/.classpath").exists() mustBe true
    new File(s"$workingDir/src").exists() mustBe true
    new File(s"$workingDir/bin").exists() mustBe true
  }

  after {
    implicit val context = new Context
    CommandLineExecutor.execute(s"rm -rf $workingDir")
  }
}
