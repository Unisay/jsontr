package com.github.unisay.jsontr

import org.json4s.JsonAST._
import org.json4s.native.JsonMethods._

object JsonTr {

  type Transform[T] = Function[T, Iterable[T]]
  val MatchPattern = """\s*\(\s*\s*match\s+([^\\"]+)\s*\)\s*""".r
  val ValueOfPattern = """\s*\(\s*\s*value\-of\s*([^\\"]+)\s*\)\s*""".r

  case class Match(path: Seq[JsonPathStep], body: JValue) {
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
        case (MatchPattern(path), body) => Some(new Match(JsonPathParser.parse(path), validateMatchBody(body)))
        case _ => None
      }
    }

    json match {
      case JObject(fields) => fields.flatMap(fieldToMatch).distinct
      case _ => throw new InvalidTemplateException("Root JSON object expected")
    }
  }

  private def rewrite(ast: JValue, fieldMapper: Transform[JField], valueMapper: Transform[JValue]) = {
    def rec(jsonValue: JValue): JValue = jsonValue match {
      case JObject(fields) => JObject(fields.map((field) => (field._1, rec(field._2))).flatMap(fieldMapper))
      case JArray(elements) => JArray(elements.flatMap(elementValue => valueMapper(rec(elementValue))))
      case x => x
    }
    rec(ast)
  }

  private def processValues(sourceAst: JValue, matchBody: JValue): JValue = {
    val fieldMapper: Transform[JField] = {
      // Key from template overrides keys from JsonPath: (value-of path) : "fieldName"
      case (ValueOfPattern(path), JString(fieldName)) => JsonPath.eval(sourceAst, path).map(it => (fieldName, it.value))
      case (ValueOfPattern(path), _: JObject) => JsonPath.eval(sourceAst, path).map(_.toJField)
      case field => List(field)
    }
    val valueMapper: Transform[JValue] = {
      case JString(ValueOfPattern(path)) => JsonPath.eval(sourceAst, path).map(_.value)
      case value => List(value)
    }
    matchBody match {
      case _: JArray | _: JObject => rewrite(matchBody, fieldMapper, valueMapper)
      case JString(ValueOfPattern(path)) => JsonPath.eval(sourceAst, path).headOption.map(_.value).getOrElse(matchBody)
      case _ => matchBody
    }
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