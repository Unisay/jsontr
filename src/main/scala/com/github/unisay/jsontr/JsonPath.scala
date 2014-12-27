package com.github.unisay.jsontr

import org.json4s.JsonAST.{JArray, JObject, JValue}

object JsonPath {

  def eval(sourceAst: JValue, pathStr: String): Seq[Node] = {
    require(sourceAst != null, "ast is null")

    val path = JsonPathParser.parse(pathStr)
    path match {
      case Seq(/, _*) => evalPath(List(Node(sourceAst)), path)
      case Seq(_: JsonPathStep, _*) => evalPath(List(Node(sourceAst)), / +: path)
      case _ => Seq.empty
    }
  }

  private def evalPath(nodes: Seq[Node], path: Seq[JsonPathStep]): Seq[Node] = {
    path match {
      case Seq() => nodes
      case Seq(step, tail@_*) => evalPath(evalStep(nodes, step), tail)
    }
  }

  private def evalStep(nodes: Seq[Node], step: JsonPathStep): Seq[Node] = {
    def arrayStep(jsonArray: JArray): Seq[Node] = step match {
      case / => List(Node(jsonArray))
      case All(_) => jsonArray.arr.map(Node(_))
      case Index(index, _) => jsonArray.arr.lift(index).map(Node(_)).toIndexedSeq
      case Prop(property, _) => Seq.empty
    }

    def objectStep(jsonObject: JObject): Seq[Node] = step match {
      case / => List(Node(jsonObject))
      case All(_) => jsonObject.obj.map(Node(_))
      case Prop(property, _) => jsonObject.obj.filter((field) => field._1 == property).map(Node(_))
      case Index(index, _) => Seq.empty
    }

    def jsonValueStep(jsonValue: JValue): Seq[Node] = jsonValue match {
      case jsonArray: JArray => arrayStep(jsonArray)
      case jsonObject: JObject => objectStep(jsonObject)
      case other => List(Node(other))
    }

    def nodeStep(node: Node): Seq[Node] = node match {
      case Node(None, jsonValue: JValue) => jsonValueStep(jsonValue)
      case Node(Some(name), jsonValue: JValue) => jsonValueStep(jsonValue)
    }

    nodes.flatMap(nodeStep)
  }

}


