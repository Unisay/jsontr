package com.github.unisay.jsontr

import com.github.unisay.jsontr.Node.Nodes
import org.json4s.JsonAST._
import org.json4s.native.JsonMethods._

object JsonTr {

  type Transform[T] = Function[T, Iterable[T]]
  val MatchPattern = """\s*\(\s*\s*match\s+([^\\"]+)\s*\)\s*""".r
  val ValueOfPattern = """\s*\(\s*\s*value\-of\s*([^\\"]+)\s*\)\s*""".r

  case class Match(pathStr: String, body: JValue) {
    override def hashCode = pathStr.hashCode()

    override def equals(that: Any): Boolean = that match {
      case o: Match => this.pathStr == o.pathStr
      case _ => false
    }
  }

  class MatchedAst(val nodes: Nodes, val jsonMatch: Match)

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
        case (MatchPattern(path), body) => Some(new Match(path, validateMatchBody(body)))
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

  private def processMatches(sourceAst: JValue, matches: Seq[Match]): Option[JValue] = {
    def processValues(sourceAst: JValue, matchedAst: MatchedAst): JValue = {
      val fieldMapper: Transform[JField] = {
        // Key from template overrides keys from JsonPath: (value-of path) : "fieldName"
        case (ValueOfPattern(path), JString(fieldName)) =>
          JsonPath.eval(path, matchedAst.nodes).map(it => (fieldName, it.jsonValue))
        case (ValueOfPattern(path), _: JObject) =>
          JsonPath.eval(path, matchedAst.nodes)
        case field => List(field)
      }
      val valueMapper: Transform[JValue] = {
        case JString(ValueOfPattern(path)) =>
          JsonPath.eval(path, matchedAst.nodes).map(_.jsonValue)
        case value => List(value)
      }
      val body: JValue = matchedAst.jsonMatch.body
      body match {
        case _: JArray | _: JObject =>
          rewrite(body, fieldMapper, valueMapper)
        case JString(ValueOfPattern(path)) =>
          JsonPath.eval(path, matchedAst.nodes).headOption.map(_.jsonValue).getOrElse(body)
        case _ => body
      }
    }

    matches
      .map(matchInstruction => new MatchedAst(JsonPath.eval(matchInstruction.pathStr, sourceAst), matchInstruction))
      .map(matchedAst => processValues(sourceAst, matchedAst)).headOption
  }

  @throws[InvalidTemplateException]("if template is invalid")
  def transform(sourceJson: String, templateJson: String): String = {
    val sourceAst = parse(sourceJson)
    val templateAst = parse(templateJson)
    val matches = extractMatches(templateAst)
    processMatches(sourceAst, matches).map({
      case jsonObject: JObject => jsonObject
      case jsonArray: JArray => jsonArray
      case jsonType => throw new JsonTransformationException(s"Resulting JSON document has $jsonType as a root")
    }).map(render).map(pretty).getOrElse("")
  }

}