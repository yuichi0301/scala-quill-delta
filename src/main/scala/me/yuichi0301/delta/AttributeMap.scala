package me.yuichi0301.delta

import spray.json._

import scala.collection.mutable.HashMap

case class AttributeMap() extends HashMap[String, Any]

object AttributeMap {

  implicit object format extends RootJsonFormat[AttributeMap] {
    override def read(jsValue: JsValue): AttributeMap = {
      val jsObject = jsValue.asInstanceOf[JsObject]
      val map = jsObject.fields.keySet.foldLeft(new AttributeMap()) { (acc, key) =>
        jsObject.fields.get(key) match {
          case Some(jsString: JsString) => acc.put(key, jsString.value)
          case Some(jsNumber: JsNumber) => acc.put(key, jsNumber.value.toInt)
          case Some(jsBoolean: JsBoolean) => acc.put(key, jsBoolean.value)
          case Some(JsNull) => acc.put(key, null)
          case value => throw new Exception(s"unknown pattern ${value}")
        }
        acc
      }
      map
    }

    override def write(map: AttributeMap): JsValue = {
      val newMap: Map[String, JsValue] = map.keySet.foldLeft(Map.empty[String, JsValue]) { (acc: Map[String, JsValue], key: String) =>
        map.get(key) match {
          case Some(value: String) => acc + (key -> JsString(value))
          case Some(value: Int) => acc + (key -> JsNumber(value))
          case Some(value: Boolean) => acc + (key -> JsBoolean(value))
          case Some(null) => acc + (key -> JsNull)
          case Some(value) => throw new Exception(s"unknown pattern ${value}")
          case None => acc
        }
      }
      JsObject(newMap)
    }
  }

  def compose(a: Option[AttributeMap], b: Option[AttributeMap], keepNull: Boolean): Option[AttributeMap] = {
    var newA: AttributeMap = if (a.isEmpty) new AttributeMap() else a.get
    var newB: AttributeMap = if (b.isEmpty) new AttributeMap() else b.get
    var attributes: AttributeMap = newB
    if (!keepNull) {
      attributes = attributes.keySet.foldLeft(new AttributeMap()) { (copy: AttributeMap, key: String) =>
        if (Option(attributes.get(key).get).isDefined) {
          copy.+=((key, attributes(key)))
        }
        copy
      }
    }
    newA.foreach {
      case (key, _) =>
        if (newA.get(key).isDefined && newB.get(key).isEmpty) {
          attributes.update(key, newA.get(key).get)
        }
    }
    if (attributes.keySet.size > 0) Some(attributes) else None
  }

  def transform(a: Option[AttributeMap], b: Option[AttributeMap], priority: Boolean = false): Option[AttributeMap] = {
    if (a.isEmpty) b
    else if (b.isEmpty) None
    else {
      if (!priority) b // b simply overwrites us without priority
      else {
        val attributes = b.get.keySet.foldLeft(new AttributeMap()) { (attrs: AttributeMap, key: String) =>
          if (!a.get.contains(key)) {
            attrs.+=((key, b.get(key)))
          }
          attrs
        }
        if (attributes.keySet.size > 0) Some(attributes) else None
      }
    }
  }

}