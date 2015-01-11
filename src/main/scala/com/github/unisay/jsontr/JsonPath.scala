package com.github.unisay.jsontr

import com.github.unisay.jsontr.MvelExpressionLang._
import com.github.unisay.jsontr.parser._

object JsonPath {

  def eval(pathStr: String, node: Node): Seq[Node] = eval(pathStr, List(node))

  def eval(pathStr: String, absoluteNodes: Seq[Node], relativeNodes: Seq[Node]): Seq[Node] = {
    if (pathStr.startsWith("/"))
      eval(pathStr, absoluteNodes)
    else
      eval(pathStr, relativeNodes)
  }

  def eval(pathStr: String, nodes: Seq[Node]): Seq[Node] = {
    require(nodes != null, "nodes is null")
    val path = JsonPathParser.parse(pathStr)
    path match {
      case Seq(Root(_), _*) => evalPath(path, nodes)
      case Seq(_: JsonPathStep, _*) => evalPath(Root() +: path, nodes)
      case _ => Seq.empty
    }
  }

  private def evalPath(path: Seq[JsonPathStep], nodes: Seq[Node]): Seq[Node] = {
    path match {
      case Seq() => nodes
      case Seq(step, tail@_*) => evalPath(tail, evalStep(step, nodes))
    }
  }

  private def evalStep(step: JsonPathStep, nodes: Seq[Node])(implicit el: ExpressionLang): Seq[Node] = {

    def nodeStep(node: Node): Seq[Node] = node match {
      case multiNode: MultiNode => step match {
        case Root(_) => List(multiNode)
        case All(_) => multiNode.children
        case Index(index, _) => multiNode.children.lift(index).toIndexedSeq
        case Prop(property, _) => multiNode.children.filter(_.nameOptional == Some(property))
      }
      case other => List(other)
    }

    def filterByPredicate(node: Node): Option[Node] = step.predicate() match {
      case None => Some(node)
      case Some(predicate) => el.applyPredicate(predicate, node)
    }

    nodes.flatMap(nodeStep).flatMap(filterByPredicate)
  }

}


