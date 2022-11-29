package me.yuichi0301.delta

import spray.json._

import scala.collection.mutable.ArrayBuffer

case class Op(insert: Option[Any] = None, delete: Option[Int] = None, retain: Option[Int] = None, attributes: Option[AttributeMap] = None)

object Op {

  implicit object Format extends RootJsonFormat[Op] {
    override def read(jsValue: JsValue) = {
      val insert: Option[Any] = jsValue.asJsObject.fields.get("insert") match {
        case Some(jsString: JsString) => Some(jsString.value)
        case Some(jsNumber: JsNumber) => Some(jsNumber.value.toInt)
        case Some(jsValue: JsValue) => Some(jsValue)
        case _ => None
      }
      val delete: Option[Int] = jsValue.asJsObject.fields.get("delete") match {
        case Some(jsNumber: JsNumber) => Some(jsNumber.value.toInt)
        case _ => None
      }
      val retain: Option[Int] = jsValue.asJsObject.fields.get("retain") match {
        case Some(jsNumber: JsNumber) => Some(jsNumber.value.toInt)
        case _ => None
      }
      val attributes: Option[AttributeMap] = jsValue.asJsObject.fields.get("attributes") match {
        case Some(arr) => Some(arr.convertTo[AttributeMap])
        case _ => None
      }
      Op(insert, delete, retain, attributes)
    }

    override def write(op: Op): JsValue = {
      val data: Map[String, JsValue] = op match {
        case Op(Some(string: String), None, None, _) => Map("insert" -> JsString(string))
        case Op(Some(number: Int), None, None, _) => Map("insert" -> JsNumber(number))
        case Op(Some(jsValue: JsValue), None, None, _) => Map("insert" -> jsValue)
        case Op(None, Some(delete), None, _) => Map("delete" -> JsNumber(delete))
        case Op(None, None, Some(retain), _) => Map("retain" -> JsNumber(retain))
        case _ => throw new Exception(s"unknown pattern ${op}")
      }
      JsObject(data + (("attributes", op.attributes.get.toJson)))
    }
  }

  def iterator(ops: ArrayBuffer[Op]) = new OpIterator(ops)

  def length(op: Op): Int = {
    op match {
      case Op(Some(string: String), None, None, _) => string.length
      case Op(Some(_: Int), None, None, _) => 1
      case Op(Some(_: JsValue), None, None, _) => 1
      case Op(None, Some(delete), None, _) => delete
      case Op(None, None, Some(retain), _) => retain
      case _ => throw new Exception(s"unknown pattern ${op}")
    }
  }

}
