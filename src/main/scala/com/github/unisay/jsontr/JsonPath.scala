package com.github.unisay.jsontr

import com.github.unisay.jsontr.MvelExpressionLang._
import com.github.unisay.jsontr.Node.Nodes
import org.json4s.JsonAST.{JArray, JObject, JValue}

object JsonPath {

  def eval(sourceAst: JValue, pathStr: String): Nodes = {
    require(sourceAst != null, "ast is null")

    val path = JsonPathParser.parse(pathStr)
    path match {
      case Seq(Root(_), _*) => evalPath(List(Node(sourceAst)), path)
      case Seq(_: JsonPathStep, _*) => evalPath(List(Node(sourceAst)), Root() +: path)
      case _ => Seq.empty
    }
  }

  private def evalPath(nodes: Nodes, path: Seq[JsonPathStep]): Nodes = {
    path match {
      case Seq() => nodes
      case Seq(step, tail@_*) => evalPath(evalStep(nodes, step), tail)
    }
  }

  private def evalStep(nodes: Nodes, step: JsonPathStep)(implicit el: ExpressionLang): Nodes = {

    def arrayStep(jsonArray: JArray): Nodes = step match {
      case Root(_) => List(Node(jsonArray))
      case All(_) => jsonArray.arr.map(Node(_))
      case Index(index, _) => jsonArray.arr.lift(index).toIndexedSeq
      case Prop(property, _) => Seq.empty
    }

    def objectStep(jsonObject: JObject): Nodes = step match {
      case Root(_) => List(Node(jsonObject))
      case All(_) => jsonObject.obj.map(Node(_))
      case Prop(property, _) => jsonObject.obj.filter((field) => field._1 == property)
      case Index(index, _) => Seq.empty
    }

    def jsonValueStep(jsonValue: JValue): Nodes = jsonValue match {
      case jsonArray: JArray => arrayStep(jsonArray)
      case jsonObject: JObject => objectStep(jsonObject)
      case other => List(Node(other))
    }

    def nodeStep(node: Node): Nodes = node match {
      case Node(None, jsonValue: JValue) => jsonValueStep(jsonValue)
      case Node(Some(name), jsonValue: JValue) => jsonValueStep(jsonValue)
    }

    def filterByPredicate(node: Node): Option[Node] = step.predicate() match {
      case None => Some(node)
      case Some(predicate) => el.applyPredicate(predicate, node)
    }

    nodes.flatMap(nodeStep).flatMap(filterByPredicate)
  }

}


