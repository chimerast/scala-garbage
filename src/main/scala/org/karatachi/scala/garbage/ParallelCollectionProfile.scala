package org.kartachi.scala.garbage

import scala.collection._
import scala.collection.parallel

import org.kartachi.scala.garbage.Profiler._
import org.kartachi.scala.garbage.ParallelCollectionProfile._

object ParallelCollectionProfile {
  val repeat = 100000

  def main(args: Array[String]): Unit = {
    println("availableProcessors: " + parallel.availableProcessors)

    profileReduceLeft
    profileForeach
    profileFor

    Profiler.output()
  }

  def profileReduceLeft(): Unit = {
    val seq = 1 to repeat
    val parseq = seq.par
    profile("Range.reduceLeft") {} {
      seq.reduceLeft { _ + _ }
    }
    profile("ParRange.reduceLeft") {} {
      parseq.reduceLeft { _ + _ }
    }
    profile("ParRange.reduce") {} {
      parseq.reduce { _ + _ }
    }
  }

  def profileForeach(): Unit = {
    val seq = 1 to repeat
    val parseq = seq.par
    profile("Range.foreach") {} {
      seq.foreach { i => fib(10) }
    }
    profile("ParRange.foreach") {} {
      parseq.foreach { i => fib(10) }
    }
    profile("ParRange.pforeach") {} {
      parseq.pforeach { i => fib(10) }
    }
  }

  def profileFor(): Unit = {
    val seq = 1 to repeat
    val parseq = seq.par
    profile("Range.for") {} {
      for (i <- seq) { fib(10) }
    }
    profile("ParRange.for") {} {
      for (i <- parseq) { fib(10) }
    }
  }

  def fib(n: Int): Int = n match {
    case 0 => 1
    case 1 => 1
    case _ => fib(n - 2) + fib(n - 1)
  }
}
