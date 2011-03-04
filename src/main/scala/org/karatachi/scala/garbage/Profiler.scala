package org.kartachi.scala.garbage

import scala.collection._

import java.nio._

import org.kartachi.scala.garbage.Profiler._

object Profile {
  def main(args: Array[String]): Unit = {
    ByteBufferProfile.run
    AnyValMutableCollectionProfile.run
    JavaCollectionProfile.run
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
    while ({i -= 1; i >= 0}) {
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
    val methods = List(".apply", ".update", ".foreach", ".prepend",  ".append")
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

object ByteBufferProfile {
  import scala.collection.JavaConversions._

  val nondirect = ByteBuffer.allocate(repeat * 4).asIntBuffer
  val direct = ByteBuffer.allocateDirect(repeat * 4).asIntBuffer

  var field = 1

  def run(): Unit = {
    nondirect.limit(nondirect.capacity)
    direct.limit(direct.capacity)

    profileApply
    profileUpdate
  }

  def profileApply(): Unit = {
    profile("java.nio.IntByffer.apply") {
      var i = -1; while ({i += 1; i < repeat}) field = nondirect.get(i)
    }
    profile("java.nio.DirectIntByffer.apply") {
      var i = -1; while ({i += 1; i < repeat}) field = direct.get(i)
    }
  }

  def profileUpdate(): Unit = {
    profile("java.nio.IntByffer.update") {
      var i = -1; while ({i += 1; i < repeat}) nondirect.put(i, field)
    }
    profile("java.nio.DirectIntByffer.update") {
      var i = -1; while ({i += 1; i < repeat}) direct.put(i, field)
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
    profileForeach
    profilePrepend
    profileAppend
    profileInsert
  }

  def profileApply(): Unit = {
    profile("Array[AnyVal].apply") {
      var i = -1; while ({i += 1; i < repeat}) field = array(i)
    }
    profile("mutable.ArrayBuffer[AnyVal].apply") {
      var i = -1; while ({i += 1; i < repeat}) field = arraybuffer(i)
    }
    profile("mutable.ListBuffer[AnyVal].apply") {
      var i = -1; while ({i += 1; i < repeat}) field = listbuffer(i)
    }
    profile("mutable.Queue[AnyVal].apply") {
      var i = -1; while ({i += 1; i < repeat}) field = queue(i)
    }
  }

  def profileUpdate(): Unit = {
    profile("Array[AnyVal].update") {
      var i = -1; while ({i += 1; i < repeat}) array(i) = field
    }
    profile("mutable.ArrayBuffer[AnyVal].update") {
      var i = -1; while ({i += 1; i < repeat}) arraybuffer(i) = field
    }
    profile("mutable.ListBuffer[AnyVal].update") {
      var i = -1; while ({i += 1; i < repeat}) listbuffer(i) = field
    }
    profile("mutable.Queue[AnyVal].update") {
      var i = -1; while ({i += 1; i < repeat}) queue(i) = field
    }
  }

  def profileForeach(): Unit = {
    profile("Array[AnyVal].foreach") {
      array.foreach(field = _)
    }
    profile("mutable.ArrayBuffer[AnyVal].foreach") {
      arraybuffer.foreach(field = _)
    }
    profile("mutable.ListBuffer[AnyVal].foreach") {
      listbuffer.foreach(field = _)
    }
    profile("mutable.Queue[AnyVal].foreach") {
      queue.foreach(field = _)
    }
  }

  def profilePrepend(): Unit = {
    val arraybuffer = new mutable.ArrayBuffer[Int]
    profile("mutable.ArrayBuffer[AnyVal].prepend") {
      var i = -1; while ({i += 1; i < repeat}) field +=: arraybuffer
    }
    val listbuffer = new mutable.ListBuffer[Int]
    profile("mutable.ListBuffer[AnyVal].prepend") {
      var i = -1; while ({i += 1; i < repeat}) field +=: listbuffer
    }
    val queue = new mutable.Queue[Int]
    profile("mutable.Queue[AnyVal].prepend") {
      var i = -1; while ({i += 1; i < repeat}) field +=: queue
    }
  }

  def profileAppend(): Unit = {
    val arraybuffer = new mutable.ArrayBuffer[Int]
    profile("mutable.ArrayBuffer[AnyVal].append") {
      var i = -1; while ({i += 1; i < repeat}) arraybuffer += field
    }
    val listbuffer = new mutable.ListBuffer[Int]
    profile("mutable.ListBuffer[AnyVal].append") {
      var i = -1; while ({i += 1; i < repeat}) listbuffer += field
    }
    val queue = new mutable.Queue[Int]
    profile("mutable.Queue[AnyVal].append") {
      var i = -1; while ({i += 1; i < repeat}) queue += field
    }
  }

  def profileInsert(): Unit = {
    val arraybuffer = new mutable.ArrayBuffer[Int]
    arraybuffer ++= array
    profile("mutable.ArrayBuffer[AnyVal].insert") {
      var i = -1; while ({i += 1; i < repeat}) arraybuffer.insert(repeat-i, field)
    }
    val listbuffer = new mutable.ListBuffer[Int]
    listbuffer ++= array
    profile("mutable.ListBuffer[AnyVal].insert") {
      var i = -1; while ({i += 1; i < repeat}) listbuffer.insert(repeat-i, field)
    }
  }
}

object JavaCollectionProfile {
  val array = Array.fill(repeat)("foo")
  val arraylist = new java.util.ArrayList[String]
  val linkedlist = new java.util.LinkedList[String]

  var field = "bar"

  def run(): Unit = {
    array.foreach(e => arraylist.add(e))
    array.foreach(e => linkedlist.add(e))

    profileApply
    profileUpdate
    profileForeach
    profilePrepend
    profileAppend
    profileInsert
  }

  def profileApply(): Unit = {
    profile("java.util.ArrayList[AnyRef].apply") {
      var i = -1; while ({i += 1; i < repeat}) field = arraylist.get(i)
    }
    profile("java.util.LinkedList[AnyRef].apply") {
      var i = -1; while ({i += 1; i < repeat}) field = linkedlist.get(i)
    }
  }

  def profileUpdate(): Unit = {
    profile("java.util.ArrayList[AnyRef].update") {
      var i = -1; while ({i += 1; i < repeat}) arraylist.set(i, field)
    }
    profile("java.util.LinkedList[AnyRef].update") {
      var i = -1; while ({i += 1; i < repeat}) linkedlist.set(i, field)
    }
  }

  def profileForeach(): Unit = {
    profile("java.util.ArrayList[AnyRef].foreach") {
      val itr = arraylist.iterator
      while (itr.hasNext) field = itr.next
    }
    profile("java.util.LinkedList[AnyRef].foreach") {
      val itr = linkedlist.iterator
      while (itr.hasNext) field = itr.next
    }
  }

  def profilePrepend(): Unit = {
    val arraylist = new java.util.ArrayList[String]
    profile("java.util.ArrayList[AnyRef].prepend") {
      var i = -1; while ({i += 1; i < repeat}) arraylist.add(0, field)
    }
    val linkedlist = new java.util.LinkedList[String]
    profile("java.util.LinkedList[AnyRef].prepend") {
      var i = -1; while ({i += 1; i < repeat}) linkedlist.add(0, field)
    }
  }

  def profileAppend(): Unit = {
    val arraylist = new java.util.ArrayList[String]
    profile("java.util.ArrayList[AnyRef].append") {
      var i = -1; while ({i += 1; i < repeat}) arraylist.add(field)
    }
    val linkedlist = new java.util.LinkedList[String]
    profile("java.util.LinkedList[AnyRef].append") {
      var i = -1; while ({i += 1; i < repeat}) linkedlist.add(field)
    }
  }

  def profileInsert(): Unit = {
    val arraylist = new java.util.ArrayList[String]
    array.foreach(e => arraylist.add(e))
    profile("java.util.ArrayList[AnyRef].insert") {
      var i = -1; while ({i += 1; i < repeat}) arraylist.add(repeat-i, field)
    }
    val linkedlist = new java.util.LinkedList[String]
    array.foreach(e => linkedlist.add(e))
    profile("java.util.LinkedList[AnyRef].insert") {
      val itr = linkedlist.listIterator(linkedlist.size)
      while (itr.hasPrevious) { itr.add(field); itr.previous; itr.previous; }
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
    profileForeach
    profilePrepend
    profileAppend
    profileInsert
  }

  def profileApply(): Unit = {
    profile("Array[AnyRef].apply") {
      var i = -1; while ({i += 1; i < repeat}) field = array(i)
    }
    profile("mutable.ArrayBuffer[AnyRef].apply") {
      var i = -1; while ({i += 1; i < repeat}) field = arraybuffer(i)
    }
    profile("mutable.ListBuffer[AnyRef].apply") {
      var i = -1; while ({i += 1; i < repeat}) field = listbuffer(i)
    }
    profile("mutable.Queue[AnyRef].apply") {
      var i = -1; while ({i += 1; i < repeat}) field = queue(i)
    }
  }

  def profileUpdate(): Unit = {
    profile("Array[AnyRef].update") {
      var i = -1; while ({i += 1; i < repeat}) array(i) = field
    }
    profile("mutable.ArrayBuffer[AnyRef].update") {
      var i = -1; while ({i += 1; i < repeat}) arraybuffer(i) = field
    }
    profile("mutable.ListBuffer[AnyRef].update") {
      var i = -1; while ({i += 1; i < repeat}) listbuffer(i) = field
    }
    profile("mutable.Queue[AnyRef].update") {
      var i = -1; while ({i += 1; i < repeat}) queue(i) = field
    }
  }

  def profileForeach(): Unit = {
    profile("Array[AnyRef].foreach") {
      array.foreach(field = _)
    }
    profile("mutable.ArrayBuffer[AnyRef].foreach") {
      arraybuffer.foreach(field = _)
    }
    profile("mutable.ListBuffer[AnyRef].foreach") {
      listbuffer.foreach(field = _)
    }
    profile("mutable.Queue[AnyRef].foreach") {
      queue.foreach(field = _)
    }
  }

  def profilePrepend(): Unit = {
    val arraybuffer = new mutable.ArrayBuffer[String]
    profile("mutable.ArrayBuffer[AnyRef].prepend") {
      var i = -1; while ({i += 1; i < repeat}) field +=: arraybuffer
    }
    val listbuffer = new mutable.ListBuffer[String]
    profile("mutable.ListBuffer[AnyRef].prepend") {
      var i = -1; while ({i += 1; i < repeat}) field +=: listbuffer
    }
    val queue = new mutable.Queue[String]
    profile("mutable.Queue[AnyRef].prepend") {
      var i = -1; while ({i += 1; i < repeat}) field +=: queue
    }
  }

  def profileAppend(): Unit = {
    val arraybuffer = new mutable.ArrayBuffer[String]
    profile("mutable.ArrayBuffer[AnyRef].append") {
      var i = -1; while ({i += 1; i < repeat}) arraybuffer += field
    }
    val listbuffer = new mutable.ListBuffer[String]
    profile("mutable.ListBuffer[AnyRef].append") {
      var i = -1; while ({i += 1; i < repeat}) listbuffer += field
    }
    val queue = new mutable.Queue[String]
    profile("mutable.Queue[AnyRef].append") {
      var i = -1; while ({i += 1; i < repeat}) queue += field
    }
  }

  def profileInsert(): Unit = {
    val arraybuffer = new mutable.ArrayBuffer[String]
    arraybuffer ++= array
    profile("mutable.ArrayBuffer[AnyRef].insert") {
      var i = -1; while ({i += 1; i < repeat}) arraybuffer.insert(repeat-i, field)
    }
    val listbuffer = new mutable.ListBuffer[String]
    listbuffer ++= array
    profile("mutable.ListBuffer[AnyRef].insert") {
      var i = -1; while ({i += 1; i < repeat}) listbuffer.insert(repeat-i, field)
    }
  }
}

object AnyRefImmutableCollectionProfile {
  val array = Array.fill(repeat)("foo")
  var list = immutable.List[String]()
  var vector = immutable.Vector[String]()

  var field = "bar"

  def run(): Unit = {
    list = array.toList
    vector = array.toIndexedSeq.asInstanceOf[Vector[String]]

    profileApply
    profileUpdate
    profileForeach
    profilePrepend
    profileAppend
  }

  def profileApply(): Unit = {
    profile("immutable.List[AnyRef].apply") {
      var i = -1; while ({i += 1; i < repeat}) field = list(i)
    }
    profile("immutable.Vector[AnyRef].apply") {
      var i = -1; while ({i += 1; i < repeat}) field = vector(i)
    }
  }

  def profileUpdate(): Unit = {
    profile("immutable.List[AnyRef].update") {
      var i = -1; while ({i += 1; i < repeat}) list = list.updated(i, field)
    }
    profile("immutable.Vector[AnyRef].update") {
      var i = -1; while ({i += 1; i < repeat}) vector = vector.updated(i, field)
    }
  }

  def profileForeach(): Unit = {
    profile("immutable.List[AnyRef].foreach") {
      list.foreach(field = _)
    }
    profile("immutable.Vector[AnyRef].foreach") {
      vector.foreach(field = _)
    }
  }

  def profilePrepend(): Unit = {
    var list = immutable.List[String]()
    profile("immutable.List[AnyRef].prepend") {
      var i = -1; while ({i += 1; i < repeat}) list +:= field
    }
    var vector = immutable.IndexedSeq[String]()
    profile("immutable.Vector[AnyRef].prepend") {
      var i = -1; while ({i += 1; i < repeat}) vector +:= field
    }
  }

  def profileAppend(): Unit = {
    var list = immutable.List[String]()
    profile("immutable.List[AnyRef].append") {
      var i = -1; while ({i += 1; i < repeat}) list :+= field
    }
    var vector = immutable.IndexedSeq[String]()
    profile("immutable.Vector[AnyRef].append") {
      var i = -1; while ({i += 1; i < repeat}) vector :+= field
    }
  }
}
