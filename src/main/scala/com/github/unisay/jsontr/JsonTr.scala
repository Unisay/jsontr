package com.github.unisay.jsontr

import com.github.unisay.jsontr.parser.NodeParser.parse
import com.github.unisay.jsontr.parser._

object JsonTr {

  type Transform[T] = Function[T, Iterable[T]]
  val TemplatePattern = """\s*~match\s+([^\\"]+)\s*""".r
  val ValueOfPattern = """\s*~value\-of\s*([^\\"]*)\s*""".r
  val ForEachPattern = """\s*~for\-each\s*([^\\"]*)\s*""".r

  case class Template(path: String, node: Node) {
    override def hashCode = path.hashCode()

    override def equals(that: Any): Boolean = that match {
      case o: Template => this.path == o.path
      case _ => false
    }
  }

  private def extractTemplates(node: Node): Seq[Template] = {
    def validate(templateNode: Node): Node = templateNode match {
      case node: MultiNode => node
      case _ => throw new InvalidTemplateException("Object or array expected as a match body")
    }

    def fieldToMatch(node: Node): Option[Template] = {
      node match {
        case Node(TemplatePattern(path)) => Some(new Template(path, validate(node).detached))
        case _ => None
      }
    }

    node match {
      case ONode(_, children) => children.flatMap(fieldToMatch).distinct
      case _ => throw new InvalidTemplateException("Root JSON object expected")
    }
  }

  private def rewrite(node: Node)(implicit nodeMapper: Transform[Node]) = {
    def rec(node: Node): Iterable[Node] = nodeMapper(node).map {
      case ANode(maybeName, children) => ANode(maybeName, children.flatMap(rec))
      case ONode(maybeName, children) => ONode(maybeName, children.flatMap(rec))
      case it => it
    }
    rec(node)
  }

  private def processTemplates(sourceNode: MultiNode, templates: Seq[Template]): Option[Node] = {
    def processTemplate(template: Template, sourceNodes: Seq[Node], matchedNodes: Seq[Node]): Node = {
      template.node match {
        case _: MultiNode =>
          rewrite(template.node) {
            case MultiNode(Some(ValueOfPattern(path)), _) =>
              JsonPath.eval(path, sourceNodes, matchedNodes)
            case SNode(Some(ValueOfPattern(_)), path) =>
              JsonPath.eval(path, sourceNodes, matchedNodes)
            case node =>
              List(node)
          }.head
        case _ => template.node
      }
    }

    templates.map(template => processTemplate(
      template = template,
      sourceNodes = List(sourceNode),
      matchedNodes = JsonPath.eval(template.path, sourceNode))
    ).headOption
  }

  @throws[InvalidTemplateException]("if template is invalid")
  def transform(sourceJson: String, templateJson: String): String = {
    val sourceMultiNode = parse(sourceJson) match {
      case node: MultiNode => node
      case _ => throw new InvalidTemplateException("Root node must be object or array")
    }
    val templateNode = parse(templateJson)
    val templates = extractTemplates(templateNode)
    processTemplates(sourceMultiNode, templates).map({
      case multiNode: MultiNode => multiNode
      case jsonType => throw new JsonTransformationException(s"Resulting JSON document has $jsonType as a root")
    }).map(NodeWriter.write).getOrElse("")
  }

}
