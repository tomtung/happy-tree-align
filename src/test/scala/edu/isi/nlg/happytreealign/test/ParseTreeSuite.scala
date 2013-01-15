package edu.isi.nlg.happytreealign.test

import org.scalatest.FunSuite
import edu.isi.nlg.happytreealign.ParseTree

class ParseTreeSuite extends FunSuite {
  test("parse English tree") {
    val expr = "( (A (B b) (C (D (E e) (F f)))) )"

    val A: ParseTree = ParseTree.parse(expr)
    assert(A.label === "A")
    assert(A.children.length === 2)
    val B = A.children(0)
    assert(B.label === "B")
    val C = A.children(1)
    assert(C.label === "C")

    assert(B.children.length === 1)
    val b = B.children(0)
    assert(b.label === "b")
    assert(b.children.isEmpty)

    assert(C.children.length === 1)
    val D = C.children(0)
    assert(D.label === "D")

    assert(D.children.length === 2)
    val E = D.children(0)
    assert(E.label === "E")
    val F = D.children(1)
    assert(F.label === "F")

    assert(E.children.length === 1)
    val e = E.children(0)
    assert(e.label === "e")
    assert(e.children.isEmpty)

    assert(F.children.length === 1)
    val f = F.children(0)
    assert(f.label === "f")
    assert(f.children.isEmpty)
  }
}
