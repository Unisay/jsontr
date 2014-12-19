package com.github.unisay.jsontr

import java.io.File

import net.javacrumbs.jsonunit.JsonAssert.assertJsonEquals
import org.scalatest._

import scala.io.Source

class JsonTrSpec extends UnitSpec with TestRegistration {

  private val testCaseDir: String = "/test-cases/"

  behavior of "JsonTr"

  val tests = for (dir <- new File(getClass.getResource(testCaseDir).toURI).listFiles) yield dir.getName
  tests.sorted.foreach { test =>
    val read = readJson(testCaseDir + test) _
    it should "verify " + test in {
      assertJsonEquals(read("target"), JsonTr.transform(read("source"), read("jsontr")))
    }
  }

  private def readJson(baseDir: String)(file: String) =
    Source.fromURL(getClass.getResource(s"$baseDir/$file.json")).mkString

}
