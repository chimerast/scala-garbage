package org.kartachi.scala.garbage

import scala.collection.mutable._

object Profiler {
  var trials = 100

  private val results = new LinkedHashMap[String, LinkedHashMap[String, Long]]()

  def profile(title: String)(init: => Unit)(block: => Unit): Unit = {
    var result = List[Long]()

    var i = trials
    while ({i -= 1; i >= 0}) {
      init
      val start = System.nanoTime
      block
      val end = System.nanoTime
      result ::= end - start
    }

    val truncate = trials / 5
    val totalTime = result.sortWith(_ < _).view(truncate, trials - truncate).sum
    val average = totalTime / (trials - truncate*2) / 1000

    println("%s: %d micro sec".format(title, average))

    val idx = title.lastIndexOf(".")
    val className = title.take(idx)
    val methodName = title.drop(idx)
    val classResults = results.getOrElseUpdate(className, new LinkedHashMap[String, Long]())

    classResults += (methodName -> average)
  }

  def output(columnsopt: Option[List[String]]=None): Unit = {
    val columns = columnsopt match {
      case Some(m) =>
        m
      case None =>
        val columns = new LinkedHashSet[String]
        results.values.foreach(_.keys.foreach(columns += _))
        columns.toList
    }
    print("|*micro sec|")
    columns.foreach(m => printf("*%s|", m))
    println
    results.keys.foreach { key =>
      print("|" + key + "|")
      columns.foreach(c => printf("%,d|", results(key).getOrElse(c, -1)))
      println
    }
  }
}
