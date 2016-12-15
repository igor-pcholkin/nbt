package com.random.nbt

import scala.io.Source
import java.io.FileReader
import scala.util.parsing.combinator.JavaTokenParsers
import com.typesafe.scalalogging.LazyLogging
import java.io.InputStreamReader

case class Phase(name: String, cmdLines: List[String], description: Option[String], dependsOn: List[String], calls: List[String],
    sets: List[String])

class Attribute

case class Command(cmdLine: String) extends Attribute
case class Call(line: String) extends Attribute
case class Set(line: String) extends Attribute
case class Description(value: String) extends Attribute
case class DependsOn(phases: List[String]) extends Attribute

class ConfigParser extends JavaTokenParsers with LazyLogging {

  lazy val eol = sys.props("line.separator")

  override val whiteSpace = """[ \t]+""".r

  def str = ("""([^\n\r])+""").r ^^ { s => s.trim() }

  def word = ("""([^\n\r \t])+""").r

  def delimitedStr = ("""([^\n\r,])+""").r ^^ { s => s.trim() }

  def phaseName = ("""([^\n\r:])+""").r

  def phaseNameHeader = phaseName <~ rep1(":", eol)

  def description = "description" ~ ":" ~> str ^^ { value =>
    Description(value)
  }

  def dependsOn = "depends on" ~ ":" ~> rep1sep(delimitedStr, ",") ^^ { values =>
    DependsOn(values)
  }

  def command = "command" ~ ":" ~> str ^^ { s => Command(s) }

  def call = "call" ~ ":" ~> str ^^ { s => Call(s) }

  def set = "set" ~ ":" ~> str ^^ { s => Set(s) }

  def attribute = (description | dependsOn | command | call | set) <~ eol

  def getCommandLines(attributes:List[Attribute]) = attributes collect { case cmd: Command => cmd.cmdLine }
  def getCalls(attributes:List[Attribute]) = attributes collect { case c: Call => c.line }
  def getSets(attributes:List[Attribute]) = attributes collect { case s: Set => s.line }
  def getValueOfFirstDescription(attributes:List[Attribute]) = (attributes collect { case d: Description => d.value }).headOption
  def getFirstDependsOnList(attributes:List[Attribute]) = (attributes collect { case d: DependsOn => d.phases }).headOption.getOrElse(Nil)

  def phase = (phaseNameHeader ~ rep(attribute)) <~ rep1(eol) ^^ {
    case name ~ attributes =>
      val cmdLines = getCommandLines(attributes)
      val description = getValueOfFirstDescription(attributes)
      val dependsOn = getFirstDependsOnList(attributes)
      val calls = getCalls(attributes)
      val sets = getSets(attributes)
      Phase(name, cmdLines, description, dependsOn, calls, sets)
  }

  def config = rep1(phase)

  def parse() = {
    parseAll(config, new InputStreamReader(getClass().getResourceAsStream("/default.conf"))) match {
      case Success(phases, _) => phases
      case ex @ _             => logger.error(ex.toString); List[Phase]()
    }
  }
}
