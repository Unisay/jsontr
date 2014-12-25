package com.github.unisay.jsontr

import org.json4s.JsonAST._
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

    "return path containing All predicate" in {
      JsonPath.parse("a/*/c") must be equalTo List(Prop("a"), All(), Prop("c"))
    }

    "return path with functions" in {
      JsonPath.parse("a/b/c|f1|f2") must be equalTo List(Prop("a"), Prop("b"), Prop("c"), Func("f1"), Func("f2"))
    }

  }

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
      |    { "name": "Forrest", "surname": "Gump"   },
      |    { "name": "Dan",     "surname": "Taylor" },
      |    { "name": "Jenny",   "surname": "Curan"  }
      |  ]
      |}
    """.stripMargin
  )

  "JsonPath.evalValue" should {
    "return a field value" in {
      JsonPath.evalValue(json, "/title") must beSome(JString("Forrest Gump"))
    }
    "return a value directly" in {
      JsonPath.evalValue(json, "/meta/authors/director") must
        beSome(JObject(("name", JString("Robert")), ("surname", JString("Zemeckis"))))
    }
  }

  "JsonPath.evalField" should {
    "return a field directly" in {
      JsonPath.evalField(json, "/title") must beSome("title", JString("Forrest Gump"))
    }
    "throw when evaluated to a value" in {
      JsonPath.evalField(json, "/") must throwA[JsonPathException]
    }
  }

  "JsonPath.eval" should {

    "require a non-null doc" in {
      JsonPath.eval(null, "/") must throwA[IllegalArgumentException]
    }

    "require a non-null path" in {
      JsonPath.eval(json, null.asInstanceOf[String]) must throwA[IllegalArgumentException] and
        (JsonPath.evalPath(json, null.asInstanceOf[Seq[Step]]) must throwA[IllegalArgumentException])
    }

    "return None for empty path" ! (JsonPath.eval(json, "") must beNone)

    "return None for blank path" ! (JsonPath.eval(json, " ") must beNone)

    "return a root document by /" in {
      JsonPath.eval(json, "/") must beSome(json)
    }

    "return a top field by absolute path" in {
      JsonPath.eval(json, "/title") must beSome(("title", JString("Forrest Gump")))
    }

    "return a top field by relative path" in {
      JsonPath.eval(json, "title") must beSome(("title", JString("Forrest Gump")))
    }

    "return a nested field by absolute path" in {
      JsonPath.eval(json, "/meta/authors/director/name") must beSome(("name", JString("Robert")))
    }

    "return a nested field by relative path" in {
      JsonPath.eval(json, "meta/authors/director/surname") must beSome(("surname", JString("Zemeckis")))
    }

    "return all object fields" in {
      JsonPath.eval(json, "meta/authors/director/*") must beSome[AnyRef](List(
        JField("name", JString("Robert")),
        JField("surname", JString("Zemeckis"))
      ))
    }

    "return array element by index" in {
      JsonPath.eval(parse( """[ "a", "b", "c" ]"""), "/2") must beSome[AnyRef](JString("c"))
    }

    "return all array elements" in {
      JsonPath.eval(parse( """[ "a", "b", "c" ]"""), "/*") must
        beSome[AnyRef](List(JString("a"), JString("b"), JString("c")))
    }

    "return by index from all elements" in {
      val json =
        """
          |[
          |  [ 1, 2, 3 ],
          |  [ 4, 5, 6 ],
          |  [ 7, 8, 9 ]
          |]
        """.stripMargin
      JsonPath.eval(parse(json), "/*/1") must beSome[AnyRef](List(JInt(2), JInt(5), JInt(8)))
    }

    "return by index from all array elements" in {
      val json =
        """
          |[
          |  [ 1, 2, 3 ],
          |  [ 4, 5, 6 ],
          |  [ 7, 8, 9 ],
          |  { "a": 10 }
          |]
        """.stripMargin
      JsonPath.eval(parse(json), "/*|isArray/1") must beSome[AnyRef](List(JInt(2), JInt(5), JInt(8)))
    }.pendingUntilFixed("Implement functions")

    "return by property from all object elements" in {
      val json =
        """
          |[
          |  [ 1, 2, 3 ],
          |  { "a": 10 },
          |  { "a": 11 },
          |]
        """.stripMargin
      JsonPath.eval(parse(json), "/*|isObject/a") must beSome[AnyRef](List(("a", JInt(10)), ("a", JInt(11))))
    }.pendingUntilFixed("Implement functions")

    "return all array elements" in {
      JsonPath.eval(json, "heroes/*") must beSome[AnyRef](List(
        JObject(
          ("name", JString("Forrest")),
          ("surname", JString("Gump"))),
        JObject(
          ("name", JString("Dan")),
          ("surname", JString("Taylor"))),
        JObject(
          ("name", JString("Jenny")),
          ("surname", JString("Curan")))
      ))
    }

    "return filtered fields" in {
      JsonPath.eval(json, "heroes/*/name|longerThan3|startsWithF", Map(
        "longerThan3" -> {
          case jsonString@JString(string) if string.length >= 3 => Some(jsonString)
          case _ => None
        },
        "startsWithF" -> {
          case jsonString@JString(string) if string.startsWith("F") => Some(jsonString)
          case _ => None
        }
      )) must beSome(Left(("name", JString("Forrest"))))
    }.pendingUntilFixed("Implement functions")

  }

}
