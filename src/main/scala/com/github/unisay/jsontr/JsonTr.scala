package com.github.unisay.jsontr

import com.github.unisay.jsontr.Node.{Nodes, nodesToJsonFields, nodesToJsonValues}
import org.json4s.JsonAST._
import org.json4s.native.JsonMethods._

object JsonTr {

  type Transform[T] = Function[T, Iterable[T]]
  val TemplatePattern = """\s*~match\s+([^\\"]+)\s*""".r
  val ValueOfPattern = """\s*~value\-of\s*([^\\"]+)\s*""".r
  val ForEachPattern = """\s*~for\-each\s*([^\\"]+)\s*""".r

  case class Template(pathStr: String, body: JValue) {
    override def hashCode = pathStr.hashCode()

    override def equals(that: Any): Boolean = that match {
      case o: Template => this.pathStr == o.pathStr
      case _ => false
    }
  }

  private def extractTemplates(json: JValue): Seq[Template] = {
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

  private def rewrite(ast: JValue)(implicit nodeMapper: Transform[Node]) = {
    def rec(node: Node): Iterable[Node] = nodeMapper(node).map {
      case Node(maybeName, jsonArray: JArray) =>
        Node(maybeName, JArray(jsonArray.arr.flatMap(jsonValue => nodesToJsonValues(rec(Node(jsonValue)))))) // todo: implicits
      case Node(maybeName, jsonObject: JObject) =>
        Node(maybeName, JObject(jsonObject.obj.flatMap(jsonField => nodesToJsonFields(rec(Node(jsonField))))))
      case it => it
    }
    rec(Node(ast))
  }

  private def processTemplates(sourceAst: JValue, templates: Seq[Template]): Option[JValue] = {
    def processValues(template: Template, sourceNodes: Nodes, matchedNodes: Nodes): JValue = {
      template.body match {
        case _: JArray | _: JObject =>
          rewrite(template.body) {
            // Key from template overrides keys from JsonPath: (value-of path) : "fieldName"
            case Node(Some(ValueOfPattern(path)), JString(fieldName)) =>
              JsonPath.eval(path, sourceNodes, matchedNodes).map(node => (fieldName, node.jsonValue))
            case Node(Some(ValueOfPattern(path)), _: JObject) =>
              JsonPath.eval(path, sourceNodes, matchedNodes)
            case Node(None, JString(ValueOfPattern(path))) =>
              JsonPath.eval(path, sourceNodes, matchedNodes).map(_.jsonValue)
            case node => List(node)
          }.head.jsonValue
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
    val matches = extractTemplates(templateAst)
    processTemplates(sourceAst, matches).map({
      case jsonObject: JObject => jsonObject
      case jsonArray: JArray => jsonArray
      case jsonType => throw new JsonTransformationException(s"Resulting JSON document has $jsonType as a root")
    }).map(render).map(pretty).getOrElse("")
  }

}
