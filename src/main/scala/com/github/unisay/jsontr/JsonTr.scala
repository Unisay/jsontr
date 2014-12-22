package com.github.unisay.jsontr

import org.json4s.JsonAST.{JNothing, JNumber, JObject}
import org.json4s.native.JsonMethods._
import org.json4s.{JField, JValue, _}

object JsonTr {

  val matchPattern = "\\s*\\(\\s*\\s*match\\s+([^\\\\\"]+)\\s*\\)\\s*".r

  case class Match(path: Seq[Step], body: JValue) {
    override def hashCode = path.hashCode()

    override def equals(that: Any): Boolean = that match {
      case o: Match => this.path == o.path
      case _ => false
    }
  }

  private def extractMatches(json: JValue): Seq[Match] = {
    def validateMatchBody(body: JValue): JValue = body match {
      case JNull => throw new InvalidTemplateException("null value is not allowed in a match body")
      case JNothing => throw new InvalidTemplateException("empty value is not allowed in a match body")
      case _: JBool => throw new InvalidTemplateException("boolean value is not allowed in a match body")
      case _: JString => throw new InvalidTemplateException("string value is not allowed in a match body")
      case _: JNumber => throw new InvalidTemplateException("numeric value is not allowed in a match body")
      case it => it
    }

    def fieldToMatch(field: JField): Option[Match] = {
      field match {
        case (matchPattern(path), body) =>
          Some(new Match(JsonPath.parse(path), validateMatchBody(body)))
        case _ => None
      }
    }

    json match {
      case JObject(fields) => fields.flatMap(fieldToMatch).distinct
      case _ => throw new InvalidTemplateException("Root JSON object expected")
    }
  }

  private def processMatchesRecursively(matches: Seq[Match]): Option[JValue] = matches match {
    case Nil => None
    case Match(_, body) :: _ => body match {
      case value: JValue => Some(value)
    }
  }

  @throws[InvalidTemplateException]("if template is invalid")
  def transform(sourceJson: String, templateJson: String): String = {
    val templateAst = parse(templateJson)
    val matches = extractMatches(templateAst)
    processMatchesRecursively(matches).map(render).map(pretty).getOrElse("")
  }

}