package com.github.unisay.jsontr

import com.github.unisay.jsontr.MvelExpressionLang._

/**
 * Parser for JsonPath expressions
 */
object JsonPathParser {

  val StepWithPredicate = """\s*([^\]\[\s]+)\s*\[([^\]\[]*)\]\s*""".r

  // todo implement using Scala Parser Combinators
  def parse(unsafePathString: String): Seq[JsonPathStep] = {
    require(unsafePathString != null, "path is null")

    val path = unsafePathString.trim
    if (path.isEmpty) return List()

    path.replaceAll("/[\\s/]*/", "/").split("/").map(parseStepWithPredicate) match {
      case Array() => List(/)
      case steps => List(steps: _*)
    }
  }

  private def parseStep(step: String, predicate: Option[String])(implicit el: ExpressionLang): JsonPathStep =
    step match {
      case it if it.forall(_.isDigit) => Index(it.toInt, predicate.map(el.compile))
      case "*" => All(predicate.map(el.compile))
      case it => Prop(it, predicate.map(el.compile))
    }

  private def parseStepWithPredicate(str: String): JsonPathStep = {
    str.trim match {
      case "" => /
      case StepWithPredicate(step, predicate) => parseStep(step, Option(predicate).filter(_.trim.nonEmpty))
      case stepWithoutPredicate => parseStep(stepWithoutPredicate, None)
    }
  }

}
