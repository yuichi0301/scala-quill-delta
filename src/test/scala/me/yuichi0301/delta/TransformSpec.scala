package me.yuichi0301.delta

import org.scalatest._
import flatspec._
import matchers._

class TransformSpec extends AnyFlatSpec with should.Matchers {

  "insert + insert" should " valid." in {
    val a1 = new Delta().insert("A")
    val b1 = new Delta().insert("B")
    val a2 = new Delta(a1.ops)
    val b2 = new Delta(b1.ops)

    val expected1 = new Delta().retain(1).insert("B")
    val expected2 = new Delta().insert("B")

    a1.transform(b1, true) should be (expected1)
    a2.transform(b2, false) should be (expected2)
  }

  "insert + retain" should " valid." in {
    val a = new Delta().insert("A")
    val attrs = new AttributeMap()
    attrs.put("bold", true)
    attrs.put("color", "red")
    val b = new Delta().retain(1, Some(attrs))

    val expectedAttrs = new AttributeMap()
    expectedAttrs.put("bold", true)
    expectedAttrs.put("color", "red")
    val expected = new Delta()
      .retain(1)
      .retain(1, Some(expectedAttrs))

    a.transform(b, true) should be (expected)
  }

  "insert + delete" should " valid." in {
    val a = new Delta().insert("A")
    val b = new Delta().delete(1)
    val expected = new Delta().retain(1).delete(1)
    a.transform(b, true) should be (expected)
  }

  "delete + insert" should " valid." in {
    val a = new Delta().delete(1)
    val b = new Delta().insert("B")
    val expected = new Delta().insert("B")
    a.transform(b, true) should be (expected)
  }

  "delete + retain" should " valid." in {
    val a = new Delta().delete(1)
    val attrs = new AttributeMap()
    attrs.put("bold", true)
    attrs.put("color", "red")
    val b = new Delta().retain(1, Some(attrs))
    val expected = new Delta()
    a.transform(b, true) should be (expected)
  }

  "delete + delete" should " valid." in {
    val a = new Delta().delete(1)
    val b = new Delta().delete(1)
    val expected = new Delta()
    a.transform(b, true) should be (expected)
  }

  "retain + insert" should " valid." in {
    val attrs = new AttributeMap()
    attrs.put("color", "blue")
    val a = new Delta().retain(1, Some(attrs))
    val b = new Delta().insert("B")
    val expected = new Delta().insert("B")
    a.transform(b, true) should be (expected)
  }

  "retain + retain" should " valid." in {
    val a1 = new Delta().retain(1, Some({val a = new AttributeMap(); a.put("color", "blue");a}))
    val b1 = new Delta().retain(1, Some({val a = new AttributeMap(); a.put("bold", true);a.put("color", "red");a}))
    val a2 = new Delta().retain(1, Some({val a = new AttributeMap(); a.put("color", "blue");a}))
    val b2 = new Delta().retain(1, Some({val a = new AttributeMap(); a.put("bold", true);a.put("color", "red");a}))
    val expected1 = new Delta().retain(1, Some({val a = new AttributeMap(); a.put("bold", true);a}))
    val expected2 = new Delta()
    a1.transform(b1, true) should be (expected1)
    b2.transform(a2, true) should be (expected2)
  }

  "retain + retain without priority" should " valid." in {
    val a1 = new Delta().retain(1, Some({val a = new AttributeMap(); a.put("color", "blue");a}))
    val b1 = new Delta().retain(1, Some({val a = new AttributeMap(); a.put("bold", true);a.put("color", "red");a}))
    val a2 = new Delta().retain(1, Some({val a = new AttributeMap(); a.put("color", "blue");a}))
    val b2 = new Delta().retain(1, Some({val a = new AttributeMap(); a.put("bold", true);a.put("color", "red");a}))
    val expected1 = new Delta().retain(1, Some({val a = new AttributeMap(); a.put("bold", true);a.put("color", "red");a}))
    val expected2 = new Delta().retain(1, Some({val a = new AttributeMap(); a.put("color", "blue");a}))
    a1.transform(b1, false) should be (expected1)
    b2.transform(a2, false) should be (expected2)
  }

  "retain + delete" should " valid." in {
    val a = new Delta().retain(1, Some({val a = new AttributeMap(); a.put("color", "blue");a}))
    val b = new Delta().delete(1)
    val expected = new Delta().delete(1)
    a.transform(b, true) should be (expected)
  }

  "alternating edits" should " valid." in {
    val a1 = new Delta()
      .retain(2)
      .insert("si")
      .delete(5)
    val b1 = new Delta()
      .retain(1)
      .insert("e")
      .delete(5)
      .retain(1)
      .insert("ow")
    val a2 = new Delta(a1.ops)
    val b2 = new Delta(b1.ops)
    val expected1 = new Delta()
      .retain(1)
      .insert("e")
      .delete(1)
      .retain(2)
      .insert("ow")
    val expected2 = new Delta()
      .retain(2)
      .insert("si")
      .delete(1)
    a1.transform(b1, false) should be (expected1)
    b2.transform(a2, false) should be (expected2)
  }

  "conflicting appends" should " valid." in {
    val a1 = new Delta().retain(3).insert("aa")
    val b1 = new Delta().retain(3).insert("bb")
    val a2 = new Delta(a1.ops)
    val b2 = new Delta(b1.ops)
    val expected1 = new Delta().retain(5).insert("bb")
    val expected2 = new Delta().retain(3).insert("aa")
    a1.transform(b1, true) should be (expected1)
    b2.transform(a2, false) should be (expected2)
  }

  "prepend + append" should " valid." in {
    val a1 = new Delta().insert("aa")
    val b1 = new Delta().retain(3).insert("bb")
    val expected1 = new Delta().retain(5).insert("bb")
    val a2 = new Delta(a1.ops)
    val b2 = new Delta(b1.ops)
    val expected2 = new Delta().insert("aa")
    a1.transform(b1, false) should be (expected1)
    b2.transform(a2, false) should be (expected2)
  }

  "trailing deletes with differing lengths" should " valid." in {
    val a1 = new Delta().retain(2).delete(1)
    val b1 = new Delta().delete(3)
    val expected1 = new Delta().delete(2)
    val a2 = new Delta(a1.ops)
    val b2 = new Delta(b1.ops)
    val expected2 = new Delta()
    a1.transform(b1, false) should be (expected1)
    b2.transform(a2, false) should be (expected2)
  }

  "immutability" should " valid." in {
    val a1 = new Delta().insert("A")
    val a2 = new Delta().insert("A")
    val b1 = new Delta().insert("B")
    val b2 = new Delta().insert("B")
    val expected = new Delta().retain(1).insert("B")
    a1.transform(b1, true) should be (expected)
    a1 should be (a2)
    b1 should be (b2)
  }

}