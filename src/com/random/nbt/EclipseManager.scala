package com.random.nbt
import scala.xml._
import java.io.PrintWriter
import java.io.FileWriter

object EclipseManager {
  def createProject(projectName: String) = {
    writeProject(projectName)
    writeClassPath()
    mkDir("src")
    mkDir("bin")
  }

  def writeProject(projectName: String) = {
    writeXML(".project",
    <projectDescription>
      <name>{projectName}</name>
      <comment></comment>
      <projects>
      </projects>
      <buildSpec>
        <buildCommand>
          <name>org.scala-ide.sdt.core.scalabuilder</name>
          <arguments>
          </arguments>
        </buildCommand>
      </buildSpec>
      <natures>
        <nature>org.scala-ide.sdt.core.scalanature</nature>
        <nature>org.eclipse.jdt.core.javanature</nature>
      </natures>
    </projectDescription>
    )
  }

  def writeClassPath() = {
    writeXML(".classpath",
    <classpath>
       <classpathentry kind="src" path="src"/>
       <classpathentry kind="src" path="conf"/>
       <classpathentry kind="con" path="org.eclipse.jdt.launching.JRE_CONTAINER"/>
       <classpathentry kind="con" path="org.scala-ide.sdt.launching.SCALA_CONTAINER"/>
       <classpathentry kind="output" path="bin"/>
    </classpath>)
  }

  def writeXML(fileName: String, xml: Elem) = {
    val pw = new PrintWriter(new FileWriter(fileName))
    pw.println("""<?xml version="1.0" encoding="UTF-8"?>""")
    val printer = new scala.xml.PrettyPrinter(80, 2)
    pw.println(printer.format(xml))
    pw.close()
  }

  def mkDir(dir: String) = {
    PhaseExecutor.executeCmdLine(s"mkdir -p $dir")
  }
}
