package com.github.unisay.jsontr

import org.json4s.JField
import org.json4s.JsonAST.{JArray, JObject, JValue}


sealed trait Step

case object / extends Step

case class All() extends Step

case class Prop(field: String) extends Step

case class Func(alias: String) extends Step

case class Index(index: Int) extends Step

object JsonPath {

  val funcPattern = "".r

  def parse(unsafePathString: String): Seq[Step] = {
    require(unsafePathString != null, "path is null")

    val path = unsafePathString.trim
    if (path.isEmpty) return List()

    def parseStep(str: String): Seq[Step] = {
      def parsePropOrIndex(s: String): Step = s match {
        case it if it.forall(_.isDigit) => Index(it.toInt)
        case it => Prop(it)
      }
      str match {
        case "" => List(/)
        case "*" => List(All())
        case s => s.split("\\|") match {
          case Array(step) => List(parsePropOrIndex(step))
          case Array(step, rest@_*) => List(parsePropOrIndex(step)) ++ rest.map(Func)
        }
      }
    }

    path.replaceAll("/[\\s/]*/", "/").split("/").map(_.trim).flatMap(parseStep) match {
      case Array() => List(/)
      case steps => List(steps: _*)
    }
  }

  def eval(sourceAst: JValue,
           pathStr: String,
           functions: Map[String, JValue => Option[JValue]] = Map.empty): Option[AnyRef] = {
    require(sourceAst != null, "ast is null")

    val path = JsonPath.parse(pathStr)

    path match {
      case Seq() => None
      case Seq(Prop(property), _*) => evalPath(sourceAst, / +: path, functions)
      case Seq(/, _*) => evalPath(sourceAst, path, functions)
      case _ => None
    }
  }

  def evalPath(sourceAst: JValue,
               path: Seq[Step],
               functions: Map[String, JValue => Option[JValue]] = Map.empty): Option[AnyRef] = {
    require(sourceAst != null, "ast is null")
    require(path != null, "path is null")

    // println(s"Eval -> path: $path, \n\t\tast: ${compact(render(sourceAst))}\n\t\tres: $res")

    path match {
      case Seq() => Some(sourceAst)
      case Seq(step) => evalStep(sourceAst, step, functions)
      case Seq(step, tail@_*) => evalStep(sourceAst, step, functions) flatMap {
        case jsonField: JField => evalPath(jsonField._2, tail, functions)
        case jsonValue: JValue => evalPath(jsonValue, tail, functions)
        case fieldSeq: Seq[Any] => Some(fieldSeq flatMap {
          case jsonValue: JValue => evalPath(jsonValue, tail, functions)
          case jsonField: JField => evalPath(jsonField._2, tail, functions)
        })
      }
    }
  }

  def evalField(sourceAst: JValue,
                pathStr: String,
                functions: Map[String, JValue => Option[JValue]] = Map.empty): Option[JField] =
    eval(sourceAst, pathStr, functions) match {
      case None => None
      case it@Some(_: JField) => it.asInstanceOf[Some[JField]]
      case other => throw new JsonPathException(s"Got result of type ${other.getClass} but Option[JField] expected")
    }

  def evalValue(sourceAst: JValue,
                pathStr: String,
                functions: Map[String, JValue => Option[JValue]] = Map.empty): Option[JValue] =
    eval(sourceAst, pathStr, functions) match {
      case None => None
      case it@Some(_: JValue) => it.asInstanceOf[Some[JValue]]
      case Some(jsonField: JField) => Some(jsonField._2)
      case other => throw new JsonPathException(s"Got result of type ${other.getClass} but Option[JValue] expected")
    }

  private def evalStep(sourceAst: JValue,
                       step: Step,
                       functions: Map[String, JValue => Option[JValue]]): Option[AnyRef] = {
    // println(s"Step -> step: $step, \n\t\tast: ${compact(render(sourceAst))}")

    (sourceAst, step) match {
      case (_, /) => Some(sourceAst)
      case (_, Func(name)) => None // TODO: implement
      case (jsonObject: JObject, All()) => Some(jsonObject.obj)
      case (jsonObject: JObject, Prop(property)) => jsonObject.obj.find((jsonField) => jsonField._1 == property)
      case (jsonArray: JArray, All()) => Some(jsonArray.arr)
      case (jsonArray: JArray, Index(index)) => Some(jsonArray.arr(index))
      case (jsonObject: JObject, Index(index)) =>
        throw new JsonPathException(s"Can't access JSON object by index: $index")
      case (jsonArray: JArray, Prop(property)) =>
        throw new JsonPathException(s"Can't access JSON array by property: $property")
      case (source, _) => Some(source)
    }
  }

}


