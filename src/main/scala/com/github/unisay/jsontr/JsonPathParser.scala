package com.github.unisay.jsontr

import com.github.unisay.jsontr.MvelExpressionLang._

import scala.util.parsing.combinator.JavaTokenParsers

sealed trait JsonPathStep {
  def predicate(): Option[Expression]
}

case class Root(predicate: Option[Expression] = None) extends JsonPathStep

case class All(predicate: Option[Expression] = None) extends JsonPathStep

case class Prop(field: String, predicate: Option[Expression] = None) extends JsonPathStep

case class Index(index: Int, predicate: Option[Expression] = None) extends JsonPathStep

object JsonPathParser extends JavaTokenParsers {

  def path: Parser[Seq[JsonPathStep]] = opt(rootStep) ~ repsep(step, """\s*/\s*""".r) ^^ {
    case Some(r) ~ p => r +: p
    case None ~ p => p
  }

  def step = indexStep | allStep | propStep

  def rootStep = "/" ~ opt(predicate) ^^ { case _ ~ optPredicate => Root(optPredicate)}

  def indexStep = index ~ opt(predicate) ^^ { case index ~ optPredicate => Index(index, optPredicate)}

  def allStep = "*" ~ opt(predicate) ^^ { case _ ~ optPredicate => All(optPredicate)}

  def propStep = property ~ opt(predicate) ^^ { case step ~ optPredicate => Prop(step, optPredicate)}

  def index = wholeNumber ^^ (_.toInt)

  def property = """[^/"\[\]]+""".r

  def predicate: Parser[Expression] = "[" ~> expression <~ "]" ^^ (expression => expression.trim)

  def expression = """[^\]]+""".r

  def parse(source: String): Seq[JsonPathStep] = {
    require(source != null)
    source.trim match {
      case "" => Seq.empty
      case s => parseAll(path, s) match {
        case Success(result, _) => result
        case NoSuccess(error, _) => throw new RuntimeException(error)
      }
    }

  }

}
