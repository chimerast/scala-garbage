package org.kartachi.scala.garbage

import scala.collection._

import org.kartachi.scala.garbage.Profiler._

object Profile {
  def main(args: Array[String]): Unit = {
    JavaCollectionProfile.run
    AnyValMutableCollectionProfile.run
    AnyRefMutableCollectionProfile.run
    AnyRefImmutableCollectionProfile.run
    Profiler.output
  }
}

object Profiler {
  val repeat = 10000
  val trials = 100
  val truncate = trials / 5

  val results = new mutable.ListMap[String, mutable.HashMap[String, Long]]()

  def profile(title: String)(block: => Unit): Unit = {
    var result = List[Long]()

    var i = trials
    while ({i -= 1; i > 0}) {
      val start = System.nanoTime
      block
      val end = System.nanoTime
      result ::= end - start
    }

    val totalTime = result.sortWith(_ < _).view(truncate, trials - truncate).reduceLeft(_ + _)
    val average = totalTime / (trials - truncate*2) / 1000

    println("%s: %d micro sec".format(title, average))

    val idx = title.lastIndexOf(".")
    val className = title.take(idx)
    val methodName = title.drop(idx)
    val classResults = results.getOrElseUpdate(className, new mutable.HashMap[String, Long]())

    classResults += (methodName -> average)
  }

  def output(): Unit = {
    val methods = List(".apply", ".update", ".append")
    print("|*micro sec|")
    methods.foreach(m => printf("*%s|", m))
    println
    results.keys.foreach { key =>
      print("|" + key + "|")
      methods.foreach(m => printf("%,d|", results(key).getOrElse(m, -1)))
      println
    }
  }
}

object JavaCollectionProfile {
  import scala.collection.JavaConversions._

  val array = Array.fill(repeat)("foo")
  val arraylist = new java.util.ArrayList[String]
  val linkedlist = new java.util.LinkedList[String]

  var field = "bar"

  def run(): Unit = {
    arraylist.addAll(array.toList)
    linkedlist.addAll(array.toList)

    profileApply
    profileUpdate
    profileAppend
  }

  def profileApply(): Unit = {
    profile("java.util.ArrayList[AnyRef].apply") {
      var i = repeat; while ({i -= 1; i > 0}) field = arraylist.get(i)
    }
    profile("java.util.LinkedList[AnyRef].apply") {
      var i = repeat; while ({i -= 1; i > 0}) field = linkedlist.get(i)
    }
  }

  def profileUpdate(): Unit = {
    profile("java.util.ArrayList[AnyRef].update") {
      var i = repeat; while ({i -= 1; i > 0}) arraylist.set(i, field)
    }
    profile("java.util.LinkedList[AnyRef].update") {
      var i = repeat; while ({i -= 1; i > 0}) linkedlist.set(i, field)
    }
  }

  def profileAppend(): Unit = {
    profile("java.util.ArrayList[AnyRef].append") {
      var i = repeat; while ({i -= 1; i > 0}) arraylist.add(field)
    }
    profile("java.util.LinkedList[AnyRef].append") {
      var i = repeat; while ({i -= 1; i > 0}) linkedlist.add(field)
    }
  }
}

object AnyRefImmutableCollectionProfile {
  val array = Array.fill(repeat)("foo")
  var list = immutable.List[String]()
  var vector = immutable.IndexedSeq[String]()

  var field = "bar"

  def run(): Unit = {
    list = array.toList
    vector = array.toIndexedSeq

    profileApply
    profileUpdate
    profileAppend
  }

  def profileApply(): Unit = {
    profile("immutable.List[AnyRef].apply") {
      var i = repeat; while ({i -= 1; i > 0}) field = list(i)
    }
    profile("immutable.Vector[AnyRef].apply") {
      var i = repeat; while ({i -= 1; i > 0}) field = vector(i)
    }
  }

  def profileUpdate(): Unit = {
    profile("immutable.List[AnyRef].update") {
      var i = repeat; while ({i -= 1; i > 0}) list = list.updated(i, field)
    }
    profile("immutable.Vector[AnyRef].update") {
      var i = repeat; while ({i -= 1; i > 0}) vector = vector.updated(i, field)
    }
  }

  def profileAppend(): Unit = {
    profile("immutable.List[AnyRef].append") {
      var i = repeat; while ({i -= 1; i > 0}) list :+= field
    }
    profile("immutable.Vector[AnyRef].append") {
      var i = repeat; while ({i -= 1; i > 0}) vector :+= field
    }
  }
}

object AnyRefMutableCollectionProfile {
  val array = Array.fill(repeat)("foo")
  val arraybuffer = new mutable.ArrayBuffer[String]
  val listbuffer = new mutable.ListBuffer[String]
  val queue = new mutable.Queue[String]

  var field = "bar"

  def run(): Unit = {
    arraybuffer ++= array
    listbuffer ++= array
    queue ++= array

    profileApply
    profileUpdate
    profileAppend
  }

  def profileApply(): Unit = {
    profile("Array[AnyRef].apply") {
      var i = repeat; while ({i -= 1; i > 0}) field = array(i)
    }
    profile("mutable.ArrayBuffer[AnyRef].apply") {
      var i = repeat; while ({i -= 1; i > 0}) field = arraybuffer(i)
    }
    profile("mutable.ListBuffer[AnyRef].apply") {
      var i = repeat; while ({i -= 1; i > 0}) field = listbuffer(i)
    }
    profile("mutable.Queue[AnyRef].apply") {
      var i = repeat; while ({i -= 1; i > 0}) field = queue(i)
    }
  }

  def profileUpdate(): Unit = {
    profile("Array[AnyRef].update") {
      var i = repeat; while ({i -= 1; i > 0}) array(i) = field
    }
    profile("mutable.ArrayBuffer[AnyRef].update") {
      var i = repeat; while ({i -= 1; i > 0}) arraybuffer(i) = field
    }
    profile("mutable.ListBuffer[AnyRef].update") {
      var i = repeat; while ({i -= 1; i > 0}) listbuffer(i) = field
    }
    profile("mutable.Queue[AnyRef].update") {
      var i = repeat; while ({i -= 1; i > 0}) queue(i) = field
    }
  }

  def profileAppend(): Unit = {
    profile("Array[AnyRef].append") {
      var i = repeat; while ({i -= 1; i > 0}) array(i) += field
    }
    profile("mutable.ArrayBuffer[AnyRef].append") {
      var i = repeat; while ({i -= 1; i > 0}) arraybuffer(i) += field
    }
    profile("mutable.ListBuffer[AnyRef].append") {
      var i = repeat; while ({i -= 1; i > 0}) listbuffer(i) += field
    }
    profile("mutable.Queue[AnyRef].append") {
      var i = repeat; while ({i -= 1; i > 0}) queue(i) += field
    }
  }
}

object AnyValMutableCollectionProfile {
  val array = Array.fill(repeat)(0)
  val arraybuffer = new mutable.ArrayBuffer[Int]
  val listbuffer = new mutable.ListBuffer[Int]
  val queue = new mutable.Queue[Int]

  var field = 1

  def run(): Unit = {
    arraybuffer ++= array
    listbuffer ++= array
    queue ++= array

    profileApply
    profileUpdate
    profileAppend
  }

  def profileApply(): Unit = {
    profile("Array[AnyVal].apply") {
      var i = repeat; while ({i -= 1; i > 0}) field = array(i)
    }
    profile("mutable.ArrayBuffer[AnyVal].apply") {
      var i = repeat; while ({i -= 1; i > 0}) field = arraybuffer(i)
    }
    profile("mutable.ListBuffer[AnyVal].apply") {
      var i = repeat; while ({i -= 1; i > 0}) field = listbuffer(i)
    }
    profile("mutable.Queue[AnyVal].apply") {
      var i = repeat; while ({i -= 1; i > 0}) field = queue(i)
    }
  }

  def profileUpdate(): Unit = {
    profile("Array[AnyVal].update") {
      var i = repeat; while ({i -= 1; i > 0}) array(i) = field
    }
    profile("mutable.ArrayBuffer[AnyVal].update") {
      var i = repeat; while ({i -= 1; i > 0}) arraybuffer(i) = field
    }
    profile("mutable.ListBuffer[AnyVal].update") {
      var i = repeat; while ({i -= 1; i > 0}) listbuffer(i) = field
    }
    profile("mutable.Queue[AnyVal].update") {
      var i = repeat; while ({i -= 1; i > 0}) queue(i) = field
    }
  }

  def profileAppend(): Unit = {
    profile("Array[AnyVal].append") {
      var i = repeat; while ({i -= 1; i > 0}) array(i) += field
    }
    profile("mutable.ArrayBuffer[AnyVal].append") {
      var i = repeat; while ({i -= 1; i > 0}) arraybuffer(i) += field
    }
    profile("mutable.ListBuffer[AnyVal].append") {
      var i = repeat; while ({i -= 1; i > 0}) listbuffer(i) += field
    }
    profile("mutable.Queue[AnyVal].append") {
      var i = repeat; while ({i -= 1; i > 0}) queue(i) += field
    }
  }
}
