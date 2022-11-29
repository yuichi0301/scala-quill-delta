package me.yuichi0301.delta

import org.scalatest._
import flatspec._
import matchers._

class ComposeSpec extends AnyFlatSpec with should.Matchers {

  "insert + insert" should " valid." in {
    val a = new Delta().insert("A")
    val b = new Delta().insert("B")
    val expected = new Delta().insert("B").insert("A")
    a.compose(b) should be (expected)
  }

  "insert + retain" should " valid." in {
    val a = new Delta().insert("A")
    val b = new Delta().retain(1, Some({val a = new AttributeMap(); a.put("bold", true); a.put("color", "red"); a.put("font", null);a}))
    val expected = new Delta().insert("A", Some({val a = new AttributeMap(); a.put("bold", true); a.put("color", "red");a}))
    a.compose(b) should be (expected)
  }

  "insert + delete" should " valid." in {
    val a = new Delta().insert("A")
    val b = new Delta().delete(1)
    val expected = new Delta()
    a.compose(b) should be (expected)
  }

  "delete + insert" should " valid." in {
    val a = new Delta().delete(1)
    val b = new Delta().insert("B")
    val expected = new Delta().insert("B").delete(1)
    a.compose(b) should be (expected)
  }

  "delete + retain" should " valid." in {
    val a = new Delta().delete(1)
    val b = new Delta().retain(1, Some({val a = new AttributeMap(); a.put("bold", true); a.put("color", "red"); a}))
    val expected = new Delta().delete(1).retain(1, Some({val a = new AttributeMap(); a.put("bold", true); a.put("color", "red"); a}))
    a.compose(b) should be (expected)
  }

  "delete + delete" should " valid." in {
    val a = new Delta().delete(1)
    val b = new Delta().delete(1)
    val expected = new Delta().delete(2)
    a.compose(b) should be (expected)
  }

  "retain + insert" should " valid." in {
    val a = new Delta().retain(1, Some({val a = new AttributeMap(); a.put("color", "blue"); a}))
    val b = new Delta().insert("B")
    val expected = new Delta().insert("B").retain(1, Some({val a = new AttributeMap(); a.put("color", "blue"); a}))
    a.compose(b) should be (expected)
  }

  "retain + retain" should " valid." in {
    val a = new Delta().retain(1, Some({val a = new AttributeMap(); a.put("color", "blue"); a}))
    val b = new Delta().retain(1, Some({val a = new AttributeMap(); a.put("bold", true); a.put("color", "blue"); a.put("font", null); a}))
    val expected = new Delta().retain(1, Some({val a = new AttributeMap(); a.put("bold", true); a.put("color", "blue"); a.put("font", null); a}))
    a.compose(b) should be (expected)
  }

  "retain + delete" should " valid." in {
    val a = new Delta().retain(1, Some({val a = new AttributeMap(); a.put("color", "blue"); a}))
    val b = new Delta().delete(1)
    val expected = new Delta().delete(1)
    a.compose(b) should be (expected)
  }

  "insert in middle of text" should " valid." in {
    val a = new Delta().insert("Hello")
    val b = new Delta().retain(3).insert("X")
    val expected = new Delta().insert("HelXlo")
    a.compose(b) should be (expected)
  }

  "insert and delete ordering" should " valid." in {
    val a = new Delta().insert("Hello")
    val b = new Delta().insert("Hello")
    val insertFirst = new Delta()
        .retain(3)
        .insert("X")
        .delete(1)
    val deleteFirst = new Delta()
      .retain(3)
      .delete(1)
      .insert("X")
    val expected = new Delta().insert("HelXo")
    a.compose(insertFirst) should be (expected)
    b.compose(deleteFirst) should be (expected)
  }

  "insert embed" should " valid." in {
    val a = new Delta().insert(1, Some({val a = new AttributeMap(); a.put("src", "http://quilljs.com/image.png"); a}))
    val b = new Delta().retain(1, Some({val a = new AttributeMap(); a.put("alt", "logo"); a}))
    val expected = new Delta().insert(1, Some({val a = new AttributeMap(); a.put("src", "http://quilljs.com/image.png"); a.put("alt", "logo"); a}))
    a.compose(b) should be (expected)
  }

  "delete entire text" should " valid." in {
    val a = new Delta().retain(4).insert("Hello")
    val b = new Delta().delete(9)
    val expected = new Delta().delete(4)
    a.compose(b) should be (expected)
  }

  "retain more than length of text" should " valid." in {
    val a = new Delta().insert("Hello")
    val b = new Delta().retain(10)
    val expected = new Delta().insert("Hello")
    a.compose(b) should be (expected)
  }

  "retain empty embed" should " valid." in {
    val a = new Delta().insert(1)
    val b = new Delta().retain(1)
    val expected = new Delta().insert(1)
    a.compose(b) should be (expected)
  }

  "remove all attributes" should " valid." in {
    val a = new Delta().insert("A", Some({val a = new AttributeMap(); a.put("bold", true); a}))
    val b = new Delta().retain(1, Some({val a = new AttributeMap(); a.put("bold", null); a}))
    val expected = new Delta().insert("A")
    a.compose(b) should be (expected)
  }

  "remove all embed attributes" should " valid." in {
    val a = new Delta().insert(2, Some({val a = new AttributeMap(); a.put("bold", true); a}))
    val b = new Delta().retain(1, Some({val a = new AttributeMap(); a.put("bold", null); a}))
    val expected = new Delta().insert(2)
    a.compose(b) should be (expected)
  }

  "immutability" should " valid." in {
    val attr1 = new AttributeMap()
    attr1.put("bold", true)
    val attr2 = new AttributeMap()
    attr2.put("bold", true)
    val a1 = new Delta().insert("Test", Some(attr1))
    val a2 = new Delta().insert("Test", Some(attr1))
    val b1 = new Delta().retain(1, Some({val a = new AttributeMap(); a.put("color", "red"); a})).delete(2)
    val b2 = new Delta().retain(1, Some({val a = new AttributeMap(); a.put("color", "red"); a})).delete(2)
    val expected = new Delta()
      .insert("T", Some({val a = new AttributeMap(); a.put("color", "red"); a.put("bold", true); a}))
        .insert("t", Some(attr1))
    a1.compose(b1) should be (expected)
    a1 should be (a2)
    b1 should be (b2)
    attr1 should be (attr2)
  }

  "retain start optimization" should " valid." in {
    val a = new Delta()
      .insert("A", Some({val a = new AttributeMap(); a.put("bold", true); a}))
      .insert("B")
      .insert("C", Some({val a = new AttributeMap(); a.put("bold", true); a}))
      .delete(1)
    val b = new Delta().retain(3).insert("D")
    val expected = new Delta()
      .insert("A", Some({val a = new AttributeMap(); a.put("bold", true); a}))
      .insert("B")
      .insert("C", Some({val a = new AttributeMap(); a.put("bold", true); a}))
      .insert("D")
      .delete(1)
    a.compose(b) should be (expected)
  }

  "retain start optimization split" should " valid." in {
    val a = new Delta()
      .insert("A", Some({val a = new AttributeMap(); a.put("bold", true); a}))
      .insert("B")
      .insert("C", Some({val a = new AttributeMap(); a.put("bold", true); a}))
      .retain(5)
      .delete(1)
    val b = new Delta().retain(4).insert("D")
    val expected = new Delta()
      .insert("A", Some({val a = new AttributeMap(); a.put("bold", true); a}))
      .insert("B")
      .insert("C", Some({val a = new AttributeMap(); a.put("bold", true); a}))
      .retain(1)
      .insert("D")
      .retain(4)
      .delete(1)
    a.compose(b) should be (expected)
  }

  "retain end optimization" should " valid." in {
    val a = new Delta()
      .insert("A", Some({val a = new AttributeMap(); a.put("bold", true); a}))
      .insert("B")
      .insert("C", Some({val a = new AttributeMap(); a.put("bold", true); a}))
    val b = new Delta().delete(1)
    val expected = new Delta()
      .insert("B")
      .insert("C", Some({val a = new AttributeMap(); a.put("bold", true); a}))
    a.compose(b) should be (expected)
  }

  "retain end optimization join" should " valid." in {
    val a = new Delta()
      .insert("A", Some({val a = new AttributeMap(); a.put("bold", true); a}))
      .insert("B")
      .insert("C", Some({val a = new AttributeMap(); a.put("bold", true); a}))
      .insert("D")
      .insert("E", Some({val a = new AttributeMap(); a.put("bold", true); a}))
      .insert("F")
    val b = new Delta().retain(1).delete(1)
    val expected = new Delta()
      .insert("AC", Some({val a = new AttributeMap(); a.put("bold", true); a}))
      .insert("D")
      .insert("E", Some({val a = new AttributeMap(); a.put("bold", true); a}))
      .insert("F")
    a.compose(b) should be (expected)
  }

}
