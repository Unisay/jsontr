package com.github.unisay.jsontr

import org.json4s.JField
import org.json4s.JsonAST.{JArray, JObject, JValue}


sealed trait Step

case object / extends Step

case class Prop(field: String) extends Step

case class Index(index: Int) extends Step

object JsonPath {

  type JResult = Either[JField, JValue]

  def parse(unsafePathString: String): Seq[Step] = {
    require(unsafePathString != null, "path is null")

    val path = unsafePathString.trim
    if (path.isEmpty) return List()

    def parseStep(str: String): Step = str match {
      case "" => /
      case s => new Prop(s)
    }

    path.replaceAll("/[\\s*/]+", "/").split("/").map(_.trim).map(parseStep) match {
      case Array() => List(/)
      case steps => List(steps: _*)
    }
  }

  def eval(ast: JValue, pathStr: String): Option[JResult] = {
    require(ast != null, "ast is null")

    val path = JsonPath.parse(pathStr)

    path match {
      case Seq() => None
      case Seq(Prop(property), _*) => evalPath(ast, / +: path)
      case Seq(/, _*) => evalPath(ast, path)
      case _ => None
    }
  }

  def evalPath(ast: JValue, path: Seq[Step]): Option[JResult] = {
    require(ast != null, "ast is null")
    require(path != null, "path is null")

    // println(s"Eval -> path: $path, \n\t\tast: ${compact(render(ast))}")

    path match {
      case Seq() => Some(Right(ast))
      case Seq(step, tail@_*) =>
        val evaluatedStep: Option[JResult] = evalStep(ast, step)
        evaluatedStep match {
          case None => None
          case Some(Left(jsonField)) if tail.nonEmpty => evalPath(jsonField._2, tail)
          case Some(Right(jsonValue)) => evalPath(jsonValue, tail)
          case other => other // todo
        }
    }
  }

  def evalStep(ast: JValue, step: Step): Option[JResult] = {
    // println(s"Step -> step: $step, \n\t\tast: ${compact(render(ast))}")

    ast match {
      case jsonObject: JObject =>
        step match {
          case / => Some(Right(ast))
          case Prop(property) =>
            jsonObject.obj
              .find((jsonField) => jsonField._1 == property)
              .map((field) => Left(field))
          case Index(index) => throw new JsonPathException(s"Can't access JSON object by index: $index")
        }
      case jsonArray: JArray =>
        step match {
          case / => Some(Right(ast))
          case Index(index) => Some(Right(jsonArray.arr(index)))
          case Prop(property) => throw new JsonPathException(s"Can't access JSON array by property: $property")
        }
      case scalar => Some(Right(ast))
    }
  }

}


