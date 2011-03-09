package org.kartachi.scala.garbage

import org.kartachi.scala.garbage.Profiler._

object ForeachProfile {
  def main(args: Array[String]): Unit = {
    Profiler.trials = 1000

    profileSingle
    profileDouble
    profileTriple

    Profiler.output()
  }

  def profileSingle(): Unit = {
    val repeat = 1000000
    var sum = 0
    profile("single.foreach") { sum = 0 } {
      (0 until repeat).foreach(
        i => sum += i)
    }
    profile("single.for") { sum = 0 } {
      for (i <- (0 until repeat)) sum += i
    }
    profile("single.while") { sum = 0 } {
      var i = 0
      while (i < repeat) {
        sum += i
        i += 1
      }
    }
  }

  def profileDouble(): Unit = {
    val repeat = 1000
    var sum = 0
    profile("double.foreach") { sum = 0 } {
      (0 until repeat).foreach(
        i => (0 until repeat).foreach(
          j => sum += j))
    }
    profile("double.for") { sum = 0 } {
      for (i <- (0 until repeat); j <- (0 until repeat))
        sum += j
    }
    profile("double.while") { sum = 0 } {
      var i = 0
      while (i < repeat) {
        var j = 0
        while (j < repeat) {
          sum += j
          j += 1
        }
        i += 1
      }
    }
  }

  def profileTriple(): Unit = {
    val repeat = 100
    var sum = 0
    profile("triple.foreach") { sum = 0 } {
      (0 until repeat).foreach(
        i => (0 until repeat).foreach(
          j => (0 until repeat).foreach(
            k => sum += k)))
    }
    profile("triple.for") { sum = 0 } {
      for (i <- (0 until repeat); j <- (0 until repeat); k <- (0 until repeat))
        sum += k
    }
    profile("triple.while") { sum = 0 } {
      var i = 0
      while (i < repeat) {
        var j = 0
        while (j < repeat) {
          var k = 0
          while (k < repeat) {
            sum += k
            k += 1
          }
          j += 1
        }
        i += 1
      }
    }
  }
}
