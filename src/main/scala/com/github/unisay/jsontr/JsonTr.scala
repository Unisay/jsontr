package com.github.unisay.jsontr

import org.json4s.JsonAST.{JNothing, JNumber, JObject, JValue}
import org.json4s._
import org.json4s.native.JsonMethods._
import org.kiama.rewriting.Rewriter._


object JsonTr {

  val matchPattern = "\\s*\\(\\s*\\s*match\\s+([^\\\\\"]+)\\s*\\)\\s*".r
  val valueOfPattern = "\\s*\\(\\s*\\s*value\\-of\\s*([^\\\\\"]+)\\s*\\)\\s*".r

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

  private def processValues(sourceAst: JValue, matchBody: JValue): JValue = {
    val rewritten = rewrite(everywheretd(rule[JField] {
      case (valueOfPattern(path), JString(valueOfKey)) => JsonPath.eval(sourceAst, path) match {
        case Some(jsonField: JField) => JField(valueOfKey, jsonField._2)
        case Some(jsonValue: JValue) => JField(valueOfKey, jsonValue)
      }
      case (valueOfPattern(path), _: JObject) => JsonPath.eval(sourceAst, path) match {
        case Some(jsonField: JField) => jsonField
        case Some(jsonValue: JValue) =>
          throw new JsonTransformationException(s"Can't insert JSON value ($jsonValue) into object ($sourceAst)")
      }

    }))(matchBody)

    rewrite(everywheretd(rule[JValue] {
      case JString(valueOfPattern(path)) => JsonPath.eval(sourceAst, path) match {
        case Some(res: JField) => res._2
        case Some(jsonValue: JValue) => jsonValue
      }
    }))(rewritten)
  }

  private def processMatchesRecursively(sourceAst: JValue, matches: Seq[Match]): Option[JValue] = matches match {
    case Nil => None
    case Match(_, body) :: _ => body match {
      case matchBody: JValue => Some(processValues(sourceAst, matchBody))
    }
  }

  @throws[InvalidTemplateException]("if template is invalid")
  def transform(sourceJson: String, templateJson: String): String = {
    val sourceAst = parse(sourceJson)
    val templateAst = parse(templateJson)
    val matches = extractMatches(templateAst)
    processMatchesRecursively(sourceAst, matches).map({
      case jsonObject: JObject => jsonObject
      case jsonArray: JArray => jsonArray
      case jsonType => throw new JsonTransformationException(s"Resulting JSON document has $jsonType as a root")
    }).map(render).map(pretty).getOrElse("")
  }

}