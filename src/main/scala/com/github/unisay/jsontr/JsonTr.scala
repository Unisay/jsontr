package com.github.unisay.jsontr

import com.github.unisay.jsontr.Node.Nodes
import org.json4s.JsonAST._
import org.json4s.native.JsonMethods._

object JsonTr {

  type Transform[T] = Function[T, Iterable[T]]
  val TemplatePattern = """\s*~match\s+([^\\"]+)\s*""".r
  val ValueOfPattern = """\s*~value\-of\s*([^\\"]+)\s*""".r

  case class Template(pathStr: String, body: JValue) {
    override def hashCode = pathStr.hashCode()

    override def equals(that: Any): Boolean = that match {
      case o: Template => this.pathStr == o.pathStr
      case _ => false
    }
  }

  private def extractMatches(json: JValue): Seq[Template] = {
    def validateMatchBody(body: JValue): JValue = body match {
      case JNull => throw new InvalidTemplateException("null value is not allowed in a match body")
      case JNothing => throw new InvalidTemplateException("empty value is not allowed in a match body")
      case _: JBool => throw new InvalidTemplateException("boolean value is not allowed in a match body")
      case _: JNumber => throw new InvalidTemplateException("numeric value is not allowed in a match body")
      case it => it
    }

    def fieldToMatch(field: JField): Option[Template] = {
      field match {
        case (TemplatePattern(path), body) => Some(new Template(path, validateMatchBody(body)))
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

  private def processTemplates(sourceAst: JValue, templates: Seq[Template]): Option[JValue] = {
    def processValues(template: Template, sourceNodes: Nodes, matchedNodes: Nodes): JValue = {
      val fieldMapper: Transform[JField] = {
        // Key from template overrides keys from JsonPath: (value-of path) : "fieldName"
        case (ValueOfPattern(path), JString(fieldName)) =>
          JsonPath.eval(path, sourceNodes, matchedNodes).map(it => (fieldName, it.jsonValue))
        case (ValueOfPattern(path), _: JObject) =>
          JsonPath.eval(path, sourceNodes, matchedNodes)
        case field => List(field)
      }
      val valueMapper: Transform[JValue] = {
        case JString(ValueOfPattern(path)) =>
          JsonPath.eval(path, sourceNodes, matchedNodes).map(_.jsonValue)
        case value => List(value)
      }
      template.body match {
        case _: JArray | _: JObject =>
          rewrite(template.body, fieldMapper, valueMapper)
        case JString(ValueOfPattern(path)) =>
          JsonPath.eval(path, sourceNodes, matchedNodes).headOption.map(_.jsonValue).getOrElse(template.body)
        case _ => template.body
      }
    }

    templates
      .map(template => (template, JsonPath.eval(template.pathStr, sourceAst)))
      .map(matched => processValues(matched._1, sourceAst, matched._2))
      .headOption
  }

  @throws[InvalidTemplateException]("if template is invalid")
  def transform(sourceJson: String, templateJson: String): String = {
    val sourceAst = parse(sourceJson)
    val templateAst = parse(templateJson)
    val matches = extractMatches(templateAst)
    processTemplates(sourceAst, matches).map({
      case jsonObject: JObject => jsonObject
      case jsonArray: JArray => jsonArray
      case jsonType => throw new JsonTransformationException(s"Resulting JSON document has $jsonType as a root")
    }).map(render).map(pretty).getOrElse("")
  }

}