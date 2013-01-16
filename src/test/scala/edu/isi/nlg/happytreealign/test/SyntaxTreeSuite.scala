package edu.isi.nlg.happytreealign.test

import org.scalatest.FunSuite
import edu.isi.nlg.happytreealign.{NonEmptySpan, SyntaxTree}

class SyntaxTreeSuite extends FunSuite {
  test("parse and construct") {
    val expr = "( (a (b e f) c (d g)) )"
    val tree = SyntaxTree.parse(expr)

    val a = tree.root
    val b :: c :: d :: Nil = a.children.toList
    val e :: f :: Nil = b.children.toList
    val g :: Nil = d.children.toList

    List(a -> "a", b -> "b", c -> "c", d -> "d", e -> "e", f -> "f", g -> "g").foreach({
      case (node, label) =>
        assert(node.label === label)
    })

    assert(tree.parentOf === Map(b -> a, c -> a, d -> a, e -> b, f -> b, g -> d))

    assert(tree.spanOf === Map(
      e -> NonEmptySpan(0, 0), f -> NonEmptySpan(1, 1), c -> NonEmptySpan(2, 2), g -> NonEmptySpan(3, 3),
      b -> NonEmptySpan(0, 1), d -> NonEmptySpan(3, 3), a -> NonEmptySpan(0, 3)))
  }

  test("toString") {
    val expr = "( (a (b e f) c (d g)) )"
    val tree = SyntaxTree.parse(expr)
    assert(expr === tree.toString)
  }

  test("breadth-first traverse") {
    val e = new SyntaxTree.Node("e")
    val f = new SyntaxTree.Node("f")
    val D = new SyntaxTree.Node("D", Vector(e))
    val B = new SyntaxTree.Node("B", Vector(D))
    val C = new SyntaxTree.Node("C", Vector(f))
    val A = new SyntaxTree.Node("A", Vector(B, C))

    assert(A.traverseBreadthFirst.toList === List(A, B, C, D, f, e))
    assert(SyntaxTree(A).traverseBreadthFirst.toList === List(A, B, C, D, f, e))
  }

  test("left-right-bottom-up traverse") {
    val e = new SyntaxTree.Node("e")
    val f = new SyntaxTree.Node("f")
    val D = new SyntaxTree.Node("D", Vector(e))
    val B = new SyntaxTree.Node("B", Vector(D))
    val C = new SyntaxTree.Node("C", Vector(f))
    val A = new SyntaxTree.Node("A", Vector(B, C))

    assert(A.traverseLeftRightBottomUp === List(e, D, f, B, C, A))
    assert(SyntaxTree(A).traverseLeftRightBottomUp === List(e, D, f, B, C, A))
  }

  test("post-order traverse") {
    val e = new SyntaxTree.Node("e")
    val f = new SyntaxTree.Node("f")
    val D = new SyntaxTree.Node("D", Vector(e))
    val B = new SyntaxTree.Node("B", Vector(D))
    val C = new SyntaxTree.Node("C", Vector(f))
    val A = new SyntaxTree.Node("A", Vector(B, C))

    assert(A.traversePostOrder.toList === List(e, D, B, f, C, A))
    assert(SyntaxTree(A).traversePostOrder.toList === List(e, D, B, f, C, A))
  }
}
