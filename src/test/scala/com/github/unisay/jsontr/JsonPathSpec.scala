package com.github.unisay.jsontr

import org.json4s.JsonAST.{JString, JValue}
import org.json4s.native.JsonMethods._
import org.specs2.mutable.Specification

class JsonPathSpec extends Specification {

  "JsonPath.parse" should {

    "require a non-null path" in {
      JsonPath.parse(null) must throwA[IllegalArgumentException]
    }

    "return empty path for empty string" in {
      JsonPath.parse("") must be equalTo Nil
    }

    "return root path for /" in {
      JsonPath.parse(" / ") must be equalTo List(/)
    }

    "compress multiple space-separated / into one" in {
      (JsonPath.parse("//") must be equalTo List(/)) and
        (JsonPath.parse("/ / /") must be equalTo List(/)) and
        (JsonPath.parse("/  /  /  /") must be equalTo List(/))
    }

    "return absolute root property path" in {
      JsonPath.parse("/prop") must be equalTo List(/, Prop("prop"))
    }

    "return relative property path" in {
      JsonPath.parse("prop") must be equalTo List(Prop("prop"))
    }

    "return absolute nested property path" in {
      JsonPath.parse("/a/b/c") must be equalTo List(/, Prop("a"), Prop("b"), Prop("c"))
    }

    "return relative nested property path" in {
      JsonPath.parse("a/b/c") must be equalTo List(Prop("a"), Prop("b"), Prop("c"))
    }

  }

  "JsonPath.eval" should {

    val json: JValue = parse(
      """
        |{
        |  "title": "Forrest Gump",
        |  "description": null,
        |  "meta": {
        |    "running-time": 142.0,
        |    "year": 1994,
        |    "best": true,
        |    "authors": {
        |      "director": {
        |        "name": "Robert",
        |        "surname": "Zemeckis"
        |      },
        |      "producer": {
        |        "name": "Wendy",
        |        "surname": "Finerman"
        |      }
        |    }
        |  },
        |  "heroes": [
        |    {
        |      "name": "Forrest",
        |      "surname": "Gump"
        |    },{
        |      "name": "Dan",
        |      "surname": "Taylor"
        |    }
        |  ]
        |}
      """.stripMargin
    )

    "require a non-null doc" in {
      JsonPath.eval(null, "/") must throwA[IllegalArgumentException]
    }

    "require a non-null path" in {
      JsonPath.eval(json, null.asInstanceOf[String]) must throwA[IllegalArgumentException] and
        (JsonPath.evalPath(json, null.asInstanceOf[Seq[Step]]) must throwA[IllegalArgumentException])
    }

    "return None for empty path" ! (JsonPath.eval(json, "") isEmpty)

    "return None for blank path" ! (JsonPath.eval(json, " ") isEmpty)

    "return a root document by /" in {
      JsonPath.eval(json, "/") must beSome(Right(json))
    }

    "return a top field by absolute path" in {
      JsonPath.eval(json, "/title") must beSome(Left(("title", JString("Forrest Gump"))))
    }

    "return a top field by relative path" in {
      JsonPath.eval(json, "title") must beSome(Left(("title", JString("Forrest Gump"))))
    }

    "return a nested field by absolute path" in {
      JsonPath.eval(json, "/meta/authors/director/name") must beSome(Left(("name", JString("Robert"))))
    }

    "return a nested field by relative path" in {
      JsonPath.eval(json, "meta/authors/director/surname") must beSome(Left(("surname", JString("Zemeckis"))))
    }

  }

}
