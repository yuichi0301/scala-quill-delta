package me.yuichi0301.delta

import scala.collection.mutable.ArrayBuffer

class OpIterator(val ops: ArrayBuffer[Op] = ArrayBuffer.empty[Op]) {

  private[this] var index = 0
  private[this] var offset = 0

  def hasNext(): Boolean = peekLength() < Int.MaxValue

  def next(length: Option[Int] = None): Op = {
    if (0 <= index && index < ops.size) {
      var newOffset = offset
      var newLength = length.getOrElse(Int.MaxValue)
      val nextOp = ops(index)
      val opLength = Op.length(nextOp)
      if (newLength >= opLength - newOffset) {
        newLength = opLength - newOffset
        index += 1
        offset = 0
      } else {
        offset += newLength
      }
      if (nextOp.delete.isDefined) {
        Op(delete = Some(newLength))
      } else {
        var retOp: Op = Op()
        if (nextOp.attributes.isDefined) {
          retOp = retOp.copy(attributes = nextOp.attributes)
        }
        if (nextOp.retain.isDefined) {
          retOp.copy(retain = Some(newLength))
        } else if (nextOp.insert.isDefined && nextOp.insert.get.isInstanceOf[String]) {
          retOp.copy(insert = Some(nextOp.insert.get.asInstanceOf[String].substring(newOffset, newOffset + newLength)))
        } else {
          // offset should === 0, length should === 1
          retOp.copy(insert = nextOp.insert)
        }
      }
    } else {
      Op(retain = Some(Int.MaxValue))
    }
  }

  def peek(): Option[Op] = if (0 <= index && index < ops.size) Some(ops(index)) else None

  def peekLength(): Int = {
    if (0 <= index && index < ops.size) {
      // Should never return 0 if our index is being managed correctly
      Op.length(ops(index)) - offset
    } else Int.MaxValue
  }

  def peekType(): String = {
    if (0 <= index && index < ops.size) {
      if (ops(index).delete.isDefined) "delete"
      else if (ops(index).retain.isDefined) "retain"
      else "insert"
    } else "retain"
  }

  def rest(): ArrayBuffer[Op] = {
    if (!hasNext()) {
      new ArrayBuffer[Op]()
    } else if (offset == 0) {
      ops.drop(index)
    } else {
      val newOffset = offset
      val newIndex = index
      val newNext = next()
      val rest = ops.drop(index)
      offset = newOffset
      index = newIndex
      val newOps = new ArrayBuffer[Op]()
      newOps += newNext
      newOps ++= rest
    }
  }

}
