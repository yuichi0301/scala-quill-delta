package me.yuichi0301.delta

import spray.json._
import spray.json.DefaultJsonProtocol._

import scala.collection.mutable.ArrayBuffer

case class Delta(ops: ArrayBuffer[Op] = ArrayBuffer.empty[Op]) {

  def insert(arg: Any, attributes: Option[AttributeMap] = None): Delta = {
    if (arg.isInstanceOf[String] && arg.asInstanceOf[String].length == 0) {
      return this
    }
    var newOp = Op(insert = Some(arg))
    if (attributes.isDefined && attributes.get.keySet.size > 0) {
      newOp = newOp.copy(attributes = Some(attributes.get))
    }
    push(newOp)
  }

  def delete(length: Int): Delta = {
    if (length <= 0) this
    else push(Op(delete = Some(length)))
  }

  def retain(length: Int, attributes: Option[AttributeMap] = None): Delta = {
    if (length <= 0) this
    else {
      var newOp: Op = Op(retain = Some(length))
      if (attributes.isDefined && attributes.get.keySet.size > 0) {
        newOp = newOp.copy(attributes = attributes)
      }
      this.push(newOp)
    }
  }

  def chop(): Delta = {
    if (ops.size != 0) {
      val lastOp = ops(ops.size - 1)
      if (lastOp.retain.isDefined && lastOp.attributes.isEmpty) {
        ops.remove(ops.size - 1)
      }
    }
    this
  }

  def push(newOp: Op): Delta = {
    var index = ops.size
    var lastOp = if (0 <= (index - 1) && (index - 1) < ops.size) Some(ops(index - 1)) else None

    if (lastOp.isDefined) {
      if (newOp.delete.isDefined && lastOp.get.delete.isDefined) {
        ops(index - 1) = Op(delete = Some(lastOp.get.delete.get + newOp.delete.get))
        return this
      }

      // Since it does not matter if we insert before or after deleting at the same index,
      // always prefer to insert first
      if (lastOp.get.delete.isDefined && newOp.insert.isDefined) {
        index -= 1
        lastOp = if ((index - 1) < ops.size) Some(ops(index - 1)) else None
        if (lastOp.isEmpty) {
          ops.+=:(newOp)
          return this
        }
      }
      if (newOp.attributes.equals(lastOp.get.attributes)) {
        if (newOp.insert.isDefined && lastOp.get.insert.isDefined && newOp.insert.get.isInstanceOf[String] && lastOp.get.insert.get.isInstanceOf[String]) {
          ops(index - 1) = Op(insert = Some(lastOp.get.insert.get.asInstanceOf[String] + newOp.insert.get.asInstanceOf[String]))
          if (newOp.attributes.isDefined) {
            ops(index - 1) = ops(index - 1).copy(attributes = newOp.attributes)
          }
          return this
        } else if (newOp.retain.isDefined && lastOp.get.retain.isDefined) {
          ops(index - 1) = Op(retain = Some(lastOp.get.retain.get + newOp.retain.get))
          if (newOp.attributes.isDefined) {
            ops(index - 1) = ops(index - 1).copy(attributes = newOp.attributes)
          }
          return this
        }
      }
    }
    if (index == ops.size) {
      ops.+=(newOp)
    } else {
      ops.insert(index, newOp)
    }
    this
  }

  def compose(other: Delta): Delta = {
    val thisIter = Op.iterator(ops)
    val otherIter = Op.iterator(other.ops)
    val newOps = ArrayBuffer.empty[Op]
    val firstOther = otherIter.peek()
    if (firstOther.isDefined &&
      firstOther.get.retain.isDefined &&
      firstOther.get.attributes.isEmpty) {
      var firstLeft = firstOther.get.retain.get
      while (thisIter.peekType() == "insert" && thisIter.peekLength() <= firstLeft) {
        firstLeft -= thisIter.peekLength()
        newOps += (thisIter.next())
      }
      if (firstOther.get.retain.get - firstLeft > 0) {
        otherIter.next(Some(firstOther.get.retain.get - firstLeft))
      }
    }
    val delta = new Delta(newOps)
    while (thisIter.hasNext() || otherIter.hasNext()) {
      if (otherIter.peekType() == "insert") {
        delta.push(otherIter.next())
      } else if (thisIter.peekType() == "delete") {
        delta.push(thisIter.next())
      } else {
        val length = Math.min(thisIter.peekLength(), otherIter.peekLength())
        val thisOp = thisIter.next(Some(length))
        val otherOp = otherIter.next(Some(length))
        if (otherOp.retain.isDefined) {
          var newOp: Op = Op()
          if (thisOp.retain.isDefined) {
            newOp = newOp.copy(retain = Some(length))
          } else {
            newOp = newOp.copy(insert = thisOp.insert)
          }
          // Preserve null when composing with a retain, otherwise remove it for inserts
          val attributes = AttributeMap.compose(
            thisOp.attributes,
            otherOp.attributes,
            thisOp.retain.isDefined
          )
          if (attributes.isDefined) {
            newOp = newOp.copy(attributes = attributes)
          }
          delta.push(newOp)
          // Optimization if rest of other is just retain
          if (
            !otherIter.hasNext() &&
              delta.ops(delta.ops.size - 1).equals(newOp)
          ) {
            val rest = new Delta(thisIter.rest())
            return delta.concat(rest).chop()
          }
        } else if (otherOp.delete.isDefined && thisOp.retain.isDefined) {
          delta.push(otherOp)
        }
      }
    }
    delta.chop()
  }

  def concat(other: Delta): Delta = {
    var delta = new Delta(ops)
    if (other.ops.size > 0) {
      delta.push(other.ops(0))
      delta = delta.copy(ops = (delta.ops ++= other.ops.drop(1)))
    }
    delta
  }

  def transform(arg: Delta, priority: Boolean = false): Delta = {
    val other: Delta = arg
    val thisIter = Op.iterator(ops)
    val otherIter = Op.iterator(other.ops)
    val delta = new Delta()
    while (thisIter.hasNext() || otherIter.hasNext()) {
      if (thisIter.peekType() == "insert" && (priority || otherIter.peekType() != "insert")) {
        delta.retain(Op.length(thisIter.next()))
      } else if (otherIter.peekType() == "insert") {
        delta.push(otherIter.next())
      } else {
        val length = Math.min(thisIter.peekLength(), otherIter.peekLength())
        val thisOp = thisIter.next(Some(length))
        val otherOp = otherIter.next(Some(length))
        if (thisOp.delete.isDefined) {
          // Our delete either makes their delete redundant or removes their retain
        } else if (otherOp.delete.isDefined) {
          delta.push(otherOp)
        } else {
          delta.retain(
            length,
            AttributeMap.transform(
              thisOp.attributes,
              otherOp.attributes,
              priority,
            )
          )
        }
      }
    }
    delta.chop()
  }

}

object Delta {


  implicit def arrayBufferFormat[T: JsonFormat] = new RootJsonFormat[ArrayBuffer[T]] {
    def write(arrayBuffer: ArrayBuffer[T]) = JsArray(arrayBuffer.map(_.toJson).toVector)

    def read(value: JsValue): ArrayBuffer[T] = value match {
      case JsArray(elements) => elements.map(_.convertTo[T]).foldLeft(ArrayBuffer.empty[T])((acc, T) => acc.addOne(T))
      case x => deserializationError("Expected ArrayBuffer as JsArray, but got " + x)
    }
  }

  implicit val format = jsonFormat1(Delta.apply)
}