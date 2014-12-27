package com.github.unisay.jsontr

import org.json4s.JField
import org.json4s.JsonAST.{JArray, JObject, JValue}


sealed trait Step {
  def functionAliases(): Seq[String]
}

case object / extends Step {
  override def functionAliases() = Seq.empty
}

case class All(functionAliases: Seq[String] = Seq.empty) extends Step

case class Prop(field: String, functionAliases: Seq[String] = Seq.empty) extends Step

case class Index(index: Int, functionAliases: Seq[String] = Seq.empty) extends Step

class Node(val maybeName: Option[String], val value: JValue) extends Tuple2[Option[String], JValue](maybeName, value) {
  def toJField: JField = this match {
    case Node(None, jsonValue: JValue) => ("", jsonValue)
    case Node(Some(thing), jsonValue: JValue) => (thing, jsonValue)
  }
}

object Node {

  def apply(name: String, value: JValue) = new Node(Some(name), value)

  def apply(value: JValue) = new Node(None, value)

  def apply(field: JField) = new Node(Some(field._1), field._2)

  def unapply(node: Node): Option[(Option[String], JValue)] = Some((node._1, node._2))
}

object JsonPath {

  def parse(unsafePathString: String): Seq[Step] = {
    require(unsafePathString != null, "path is null")

    val path = unsafePathString.trim
    if (path.isEmpty) return List()

    def parseStep(str: String): Step = {
      str.trim match {
        case "" => /
        case s => s.split("\\|") match {
          case Array(step, rest@_*) => step match {
            case it if it.forall(_.isDigit) => Index(it.toInt, rest.map(_.trim))
            case "*" => All(rest.map(_.trim))
            case it => Prop(it, rest.map(_.trim))
          }
        }
      }
    }

    path.replaceAll("/[\\s/]*/", "/").split("/").map(parseStep) match {
      case Array() => List(/)
      case steps => List(steps: _*)
    }
  }

  def eval(sourceAst: JValue, pathStr: String): Seq[Node] = {
    require(sourceAst != null, "ast is null")

    val path = JsonPath.parse(pathStr)
    path match {
      case Seq(/, _*) => evalPath(List(Node(sourceAst)), path)
      case Seq(_: Step, _*) => evalPath(List(Node(sourceAst)), / +: path)
      case _ => Seq.empty
    }
  }

  private def evalPath(nodes: Seq[Node], path: Seq[Step]): Seq[Node] = {
    // println(s"Eval -> path: $path, \n\t\tast: ${compact(render(sourceAst))}\n\t\tres: $res")
    path match {
      case Seq() => nodes
      case Seq(step, tail@_*) => evalPath(evalStep(nodes, step), tail)
    }
  }

  private def evalStep(nodes: Seq[Node], step: Step): Seq[Node] = {
    // println(s"Step -> step: $step, \n\t\tast: ${compact(render(sourceAst))}")

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


