package com.github.unisay.jsontr

import com.github.unisay.jsontr.JsonTr.transform
import net.javacrumbs.jsonunit.JsonAssert.assertJsonEquals
import org.specs2.mutable.Specification

class JsonTrSpec extends Specification {

  "JsonTr" should {

    val json =
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

    "throw exception for invalid template" in {
      transform(json, "[]") must throwA[InvalidTemplateException]
    }

    "produce blank document when no root matched" in {
      transform(json, """ { "a": "b" } """) must be empty
    }

    "handle duplicate matches" in {
      val replacement = """ { "a": "b" } """
      val template = s""" { "(match /)": $replacement, "(match /)": []} """
      assertJsonEquals(replacement, transform(json, template)) must not throwA
    }

    "match root, replace it with array" in {
      val replacement = """ [ "a", "b", "c" ] """
      val template = s""" { "(match /)": $replacement } """
      assertJsonEquals(replacement, transform(json, template)) must not throwA
    }

    "match root, replace it with object" in {
      val replacement = """ { "a": "b" } """
      val template = s""" { "(match /)": $replacement } """
      assertJsonEquals(replacement, transform(json, template)) must not throwA
    }

    "match root, do not replace with null" in {
      val template = """ { "(match /)": null } """
      transform(json, template) must throwA[InvalidTemplateException]
    }

    "match root, do not replace with bool" in {
      transform(json, """ { "(match /)": true } """) must throwA[InvalidTemplateException]
    }

    "match root, do not replace with string" in {
      val template = """ { "(match /)": "foo" } """
      transform(json, template) must throwA[InvalidTemplateException]
    }

    "match root, do not replace with number" in {
      val template = """ { "(match /)": 1 } """
      transform(json, template) must throwA[InvalidTemplateException]
    }

    "match root, copy value-of absolute property path into object, keep original property key" in {
      val source =
        """
          |{
          |  "a": 1,
          |  "b": 2
          |}
        """.stripMargin
      val template =
        """
          |{
          |  "(match /)": {
          |    "(value-of /a)" : {}
          |  }
          |}
          | """.stripMargin
      val expected =
        """
          |{
          |  "a": 1
          |}
        """.stripMargin
      assertJsonEquals(expected, transform(source, template)) must not throwA
    }

    "match root, copy value-of absolute property path into object, replace original property key" in {
      val source =
        """
          |{
          |  "a": 1,
          |  "b": 2
          |}
        """.stripMargin
      val template =
        """
          |{
          |  "(match /)": {
          |    "(value-of /a)" : "other-key"
          |  }
          |}
          | """.stripMargin
      val expected =
        """
          |{
          |  "other-key": 1
          |}
        """.stripMargin
      assertJsonEquals(expected, transform(source, template)) must not throwA
    }

    "match root, copy value-of relative property path into object" in {
      val source =
        """
          |{
          |  "a": {
          |    "b": 1
          |  },
          |  "c": 2
          |}
        """.stripMargin
      val template =
        """
          |{
          |  "(match /)": {
          |    "(value-of a/b)" : {}
          |  }
          |}
        """.stripMargin
      val expected =
        """
          |{
          |  "b": 1
          |}
        """.stripMargin
      assertJsonEquals(expected, transform(source, template)) must not throwA
    }

    "match root, copy value-of absolute property path into array" in {
      val source =
        """
          |{
          |  "a": 1,
          |  "b": 2
          |}
        """.stripMargin
      val template =
        """
          |{
          |  "(match /)": [
          |    "(value-of /a)"
          |  ]
          |}
        """.stripMargin
      assertJsonEquals("[1]", transform(source, template)) must not throwA
    }

    "match root, copy value-of relative property path into array" in {
      val source =
        """
          |{
          |  "a": 1,
          |  "b": 2
          |}
        """.stripMargin
      val template =
        """
          |{
          |  "(match /)": [
          |    "(value-of /a)"
          |  ]
          |}
        """.stripMargin
      assertJsonEquals("[1]", transform(source, template)) must not throwA
    }

  }

}