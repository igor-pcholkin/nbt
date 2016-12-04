package com.random.nbt

import java.io.File
import scala.reflect.internal.util.BatchSourceFile
import scala.io.Source
import scala.annotation.tailrec
import java.net.URL
import scala.collection.mutable.Map

object ScalaCompiler {
  val ivyHelper = new IvyHelper()
  val org = "org.scala-lang"
  val scalaCompiler = "scala-compiler"
  val scalaReflect = "scala-reflect"
}

class ScalaCompiler(implicit val context: Map[String, Any]) extends FileUtils {
  import ScalaCompiler._

  def compile(sourceDir: String)(implicit context: Map[String, Any]) = {
    val scalaVersion = context.get("scalaVersion").asInstanceOf[Option[String]]
    val mayBeScalaCompilerVersion = scalaVersion.orElse(ivyHelper.getLastLocalVersion(org, scalaCompiler))
    val mayBeScalaReflectVersion = scalaVersion.orElse(ivyHelper.getLastLocalVersion(org, scalaReflect))
    (mayBeScalaCompilerVersion, mayBeScalaReflectVersion) match {
      case (Some(scalaCompilerVersion), Some(scalaReflectVersion)) =>
        if (scalaCompilerVersion == scalaReflectVersion) {
          compileUsing(sourceDir, scalaCompilerVersion)
        } else {
          println(s"Error: latest scala-compiler ($scalaCompilerVersion) and scala-reflect ($scalaReflectVersion) version mismatch")
        }
      case _ => println("Error: No scala compiler or dependant libs found")
    }
  }

  def compileUsing(sourceDir: String, scalaVersion: String): Unit = {
    val scalaCompilerJarPath = ivyHelper.getModuleJarFileName(org, scalaCompiler, scalaVersion)
    val scalaReflectJarPath = ivyHelper.getModuleJarFileName(org, scalaReflect, scalaVersion)
    val classPath = List(scalaCompilerJarPath, scalaReflectJarPath) map { jarPath =>
      new URL(s"file://$jarPath")
    }
    val classLoader = new java.net.URLClassLoader(classPath.toArray)
    println(s"""Compile using classpath: ${classPath.mkString(":")}""")
    compileUsing(sourceDir, classLoader)
  }

  def getDependenciesAsJarPaths() = {
    context.get("compileDependencies") match {
      case Some(compileDependencies) => parseDependencies(compileDependencies.asInstanceOf[Array[String]]) flatMap { case (org, module) =>
        ivyHelper.getLastLocalVersionFilePath(org, module)
      }
      case None => Nil
    }
  }

  def parseDependencies(rawDependencies: Seq[String]) = {
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

  def compileUsing(sourceDir: String, classLoader: ClassLoader) = {

    val settingsClass = classLoader.loadClass("scala.tools.nsc.Settings")
    val settingsInstance = settingsClass.getConstructor().newInstance().asInstanceOf[AnyRef]

    addDependenciesToCompile(settingsClass, settingsInstance, classLoader)

    val reporterClass = classLoader.loadClass("scala.tools.nsc.reporters.ConsoleReporter")
    val reporterInstance = reporterClass.getConstructor(settingsClass).newInstance(settingsInstance).asInstanceOf[AnyRef]

    val globalClass = classLoader.loadClass("scala.tools.nsc.Global")
    val abstractReporterClass = classLoader.loadClass("scala.tools.nsc.reporters.Reporter")
    val globalContructor = globalClass.getConstructor(settingsClass, abstractReporterClass)
    val globalInstance = globalContructor.newInstance(settingsInstance, reporterInstance).asInstanceOf[AnyRef]

    invokeCompileMethod(sourceDir, globalClass, globalInstance, classLoader)
  }

  def addDependenciesToCompile(settingsClass: Class[_], settingsInstance: AnyRef, classLoader: ClassLoader) = {
    val classPath = getDependenciesAsJarPaths.mkString(":")
    val cpArgs = s"""-classpath "$classPath" """
    println(s"Passing additional dependencies for compiler: $cpArgs")
    val processArgumentStringMethod = settingsClass.getMethod("processArgumentString", classLoader.loadClass("java.lang.String"))
    processArgumentStringMethod.invoke(settingsInstance, cpArgs)
  }

  def invokeCompileMethod(sourceDir: String, globalClass: Class[_], globalInstance: AnyRef, classLoader: ClassLoader) = {
    val runClass = globalInstance.getClass().getClasses.find { c =>
      c.getName.contains("Run")
    } match {
      case Some(runClass) => runClass
      case None           => throw new RuntimeException("No Run class found")
    }
    val runInstance = runClass.getConstructor(globalClass).newInstance(globalInstance).asInstanceOf[AnyRef]
    val compileMethod = runClass.getMethod("compileSources", classLoader.loadClass("scala.collection.immutable.List"))
    val sources = prepareSources(sourceDir)
    val runArgs = Array(sources.toList)
    compileMethod.invoke(runInstance, runArgs: _*)
  }

  def prepareSources(sourceDir: String) = scanSourceFilesInDir(sourceDir) map { fileName =>
    println("Compiling file: " + fileName)
    val fileContents = Source.fromFile(fileName).getLines().mkString("\n")
    new BatchSourceFile(fileName, fileContents)
  }

  def scanSourceFilesInDir(sourceDir: String): List[String] = scanFilesInDir(sourceDir, fileName => fileName.endsWith(".scala"))
}
