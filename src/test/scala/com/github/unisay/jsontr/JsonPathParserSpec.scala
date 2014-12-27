package com.github.unisay.jsontr

import org.specs2.mutable.Specification

class JsonPathParserSpec extends Specification {

  "JsonPathParser.parse" should {

    "require a non-null path" in {
      JsonPathParser.parse(null) must throwA[IllegalArgumentException]
    }

    "return empty path for empty string" in {
      JsonPathParser.parse("") must be equalTo Nil
    }

    "return root path for /" in {
      JsonPathParser.parse(" / ") must be equalTo List(/)
    }

    "compress multiple space-separated / into one" in {
      (JsonPathParser.parse("//") must be equalTo List(/)) and
        (JsonPathParser.parse("/ / /") must be equalTo List(/)) and
        (JsonPathParser.parse("/  /  /  /") must be equalTo List(/))
    }

    "return absolute root property path" in {
      JsonPathParser.parse("/prop") must be equalTo List(/, Prop("prop"))
    }

    "return absolute element path" in {
      JsonPathParser.parse("/1") must be equalTo List(/, Index(1))
    }

    "return relative property path" in {
      JsonPathParser.parse("prop") must be equalTo List(Prop("prop"))
    }

    "return relative element path" in {
      JsonPathParser.parse("1") must be equalTo List(Index(1))
    }

    "return absolute nested property path" in {
      JsonPathParser.parse("/a/b/c") must be equalTo List(/, Prop("a"), Prop("b"), Prop("c"))
    }

    "return relative nested property path" in {
      JsonPathParser.parse("a/b/c") must be equalTo List(Prop("a"), Prop("b"), Prop("c"))
    }

    "return absolute nested mixed path" in {
      JsonPathParser.parse("/a/1/2") must be equalTo List(/, Prop("a"), Index(1), Index(2))
    }

    "return relative nested mixed path" in {
      JsonPathParser.parse("a/1/2") must be equalTo List(Prop("a"), Index(1), Index(2))
    }

    "return path containing *" in {
      JsonPathParser.parse("a/*/c") must be equalTo List(Prop("a"), All(), Prop("c"))
    }

    "return path with empty predicate" in {
      JsonPathParser.parse("a[]") must be equalTo List(Prop("a", None))
    }

    "return path with predicates" in {
      def stepMatcher(name: String)(step: JsonPathStep) = step match {
        case Prop(`name`, Some(_)) => true
        case _ => false
      }
      JsonPathParser.parse("a[p1 < p2 && p3]/b/c[p2.name]") must contain(exactly[JsonPathStep](
        stepMatcher("a") _, Prop("b", None), stepMatcher("c") _))
    }

  }
}
