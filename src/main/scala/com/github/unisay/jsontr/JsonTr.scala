package com.github.unisay.jsontr

import com.github.unisay.jsontr.parser.NodeParser.parse
import com.github.unisay.jsontr.parser._

object JsonTr {

  type Transform[T] = Function[T, Iterable[T]]
  val TemplatePattern = """\s*~match\s+([^\\"]+)\s*""".r
  val ValueOfPattern = """\s*~value\-of\s*([^\\"]+)\s*""".r
  val ForEachPattern = """\s*~for\-each\s*([^\\"]+)\s*""".r

  case class Template(path: String, node: Node) {
    override def hashCode = path.hashCode()

    override def equals(that: Any): Boolean = that match {
      case o: Template => this.path == o.path
      case _ => false
    }
  }

  private def extractTemplates(node: Node): Seq[Template] = {
    def validateTemplateNode(templateNode: Node): Node = templateNode match {
      case _: NullNode => throw new InvalidTemplateException("null value is not allowed in a match body")
      case _: BNode => throw new InvalidTemplateException("boolean value is not allowed in a match body")
      case _: INode | _: DNode => throw new InvalidTemplateException("numeric value is not allowed in a match body")
      case it => it
    }

    def fieldToMatch(node: Node): Option[Template] = {
      node match {
        case Node(TemplatePattern(path)) => Some(new Template(path, validateTemplateNode(node)))
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
    def processValues(template: Template, sourceNodes: Seq[Node], matchedNodes: Seq[Node]): Node = {
      template.node match {
        case _: MultiNode =>
          rewrite(template.node) {
            // Key from template overrides keys from JsonPath: (value-of path) : "fieldName"
            case SNode(Some(ValueOfPattern(path)), fieldName) =>
              JsonPath.eval(path, sourceNodes, matchedNodes)
            case MultiNode(Some(ValueOfPattern(path)), _) =>
              JsonPath.eval(path, sourceNodes, matchedNodes)
            case node => List(node)
          }.head
        case SNode(Some(ValueOfPattern(path)), _) =>
          JsonPath.eval(path, sourceNodes, matchedNodes).headOption.getOrElse(template.node)
        case _ => template.node
      }
    }

    templates
      .map(template => (template, JsonPath.eval(template.path, sourceNode)))
      .map(matched => processValues(matched._1, sourceNode.children, matched._2))
      .headOption
  }

  @throws[InvalidTemplateException]("if template is invalid")
  def transform(sourceJson: String, templateJson: String): String = {
    val sourceMultiNode = parse(sourceJson) match {
      case node: MultiNode => node
      case _ => throw new InvalidTemplateException("Root node must be object or array")
    }
    val templateNode = parse(templateJson)
    val matches = extractTemplates(templateNode)
    processTemplates(sourceMultiNode, matches).map({
      case multiNode: MultiNode => multiNode
      case jsonType => throw new JsonTransformationException(s"Resulting JSON document has $jsonType as a root")
    }).map(NodeWriter.write).getOrElse("")
  }

}
