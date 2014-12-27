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
      JsonPath.parse("a/b/c|f1|f2") must be equalTo List(Prop("a"), Prop("b"), Prop("c", List("f1", "f2")))
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

  "JsonPath.eval" should {

    "require a non-null doc" in {
      JsonPath.eval(null, "/") must throwA[IllegalArgumentException]
    }

    "require a non-null path" in {
      JsonPath.eval(json, null.asInstanceOf[String]) must throwA[IllegalArgumentException]
    }

    "return None for empty path" ! (JsonPath.eval(json, "") must be empty)

    "return None for blank path" ! (JsonPath.eval(json, " ") must be empty)

    "return a root document by /" in {
      JsonPath.eval(json, "/") must contain(exactly(Node(json)))
    }

    "return a top field by absolute path" in {
      JsonPath.eval(json, "/title") must contain(exactly(Node("title", JString("Forrest Gump"))))
    }

    "return a top field by relative path" in {
      JsonPath.eval(json, "title") must contain(exactly(Node("title", JString("Forrest Gump"))))
    }

    "return a nested field by absolute path" in {
      JsonPath.eval(json, "/meta/authors/director/name") must contain(exactly(Node("name", JString("Robert"))))
    }

    "return a nested field by relative path" in {
      JsonPath.eval(json, "meta/authors/director/surname") must contain(exactly(Node("surname", JString("Zemeckis"))))
    }

    "return all object fields" in {
      JsonPath.eval(json, "meta/authors/director/*") must contain(exactly(
        Node("name", JString("Robert")),
        Node("surname", JString("Zemeckis"))
      ))
    }

    "return array element by index" in {
      JsonPath.eval(parse( """[ "a", "b", "c" ]"""), "/2") must contain(exactly(Node(JString("c"))))
    }

    "return all array elements" in {
      JsonPath.eval(parse( """[ "a", "b", "c" ]"""), "/*") must contain(exactly(
        Node(JString("a")),
        Node(JString("b")),
        Node(JString("c"))
      ))
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
      JsonPath.eval(parse(json), "/*/1") must contain(exactly(Node(JInt(2)), Node(JInt(5)), Node(JInt(8))))
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
      JsonPath.eval(parse(json), "/*|isArray/1") must contain(exactly(Node(JInt(2)), Node(JInt(5)), Node(JInt(8))))
    }

    "return by property from all object elements" in {
      val json =
        """
          |[
          |  [ 1, 2, 3 ],
          |  { "a": 10 },
          |  { "a": 11 },
          |]
        """.stripMargin
      JsonPath.eval(parse(json), "/*|isObject/a") must contain(exactly(Node("a", JInt(10)), Node("a", JInt(11))))
    }

    "return all array elements" in {
      JsonPath.eval(json, "heroes/*") must contain(exactly(
        Node(JObject(("name", JString("Forrest")), ("surname", JString("Gump")))),
        Node(JObject(("name", JString("Dan")), ("surname", JString("Taylor")))),
        Node(JObject(("name", JString("Jenny")), ("surname", JString("Curan"))))
      ))
    }

    "return filtered fields" in {
      JsonPath.eval(json, "heroes/*/name[mvel expr]") must contain(exactly(Node(JString("Forrest"))))
    }.pendingUntilFixed("Implement MVEl")

  }

}
