package edu.isi.nlg.happytreealign.test

import org.scalatest.FunSuite
import edu.isi.nlg.happytreealign._
import edu.isi.nlg.happytreealign.trans._
import Direction._

class TransformationSuite extends FunSuite {
  test("apply articulate transformation") {
    val tree1 = SyntaxTree.parse("( (S (NP (DUMMY Other) members) (VP will (DUMMY arrive) in two groups) (. .)) )")
    assert(Articulate(parentLabel = "S", leftLabel = "NP", rightLabel = "VP")(tree1).get.toString ===
      "( (S (NP+VP (NP (DUMMY Other) members) (VP will (DUMMY arrive) in two groups)) (. .)) )")

    val tree2 = SyntaxTree.parse("( (a (a (b x) (c x) x) b c) )")
    val newTree2 = Articulate("a", "b", "c")(tree2).get
    assert(newTree2.toString ===
      "( (a (a (b+c (b x) (c x)) x) (b+c b c)) )")

    assert(Articulate("a+b", "a", "b")(newTree2).isEmpty)

    val tree3 = SyntaxTree.parse("( (a b c) )")
    assert(Articulate("a", "b", "c")(tree3).isEmpty)
  }

  test("extract articulate transformations") {
    val tree = SyntaxTree.parse("( (a (b x x x) (a b (a x)) c) )")
    assert(Articulate.extract(tree) === Set(
      Articulate("a", "b", "a"),
      Articulate("a", "a", "c")
    ))

    val newTree = Articulate(parentLabel = "a", leftLabel = "b", rightLabel = "a")(tree).get
    assert(Articulate.extract(newTree) === Set.empty)
  }

  test("apply flatten transformation") {
    val tree1 = SyntaxTree.parse("( (NP (DT the) (NNP1 China) (NML (NNP Trade) (NNP Promotion)) (NNP2 Council)) )")
    assert(Flatten("NP", "NML")(tree1).get.toString ===
      "( (NP (DT the) (NNP1 China) (NNP Trade) (NNP Promotion) (NNP2 Council)) )")

    assert(
      Flatten("NP", "NML", Some(("NNP1", Right)))(tree1).get.toString ===
        "( (NP (DT the) (NNP1 China) (NNP Trade) (NNP Promotion) (NNP2 Council)) )")
    assert(
      Flatten("NP", "NML", Some(("NNP2", Left)))(tree1).get.toString ===
        "( (NP (DT the) (NNP1 China) (NNP Trade) (NNP Promotion) (NNP2 Council)) )")

    assert(Flatten("NP", "NML", Some(("NNP1", Left)))(tree1).isEmpty)
    assert(Flatten("NP", "NML", Some(("NNP2", Right)))(tree1).isEmpty)

    val tree2 = SyntaxTree.parse("( (a (b x (b x (b x x))) c) )")
    assert(Flatten("a", "b", Some("c" -> Left))(tree2).get.toString ===
      "( (a x x (b x x) c) )")
  }

  test("extract flatten transformations") {
    val tree = SyntaxTree.parse("( (a (b d (e (k m) l)) (c f g) (d h i j)) )")
    assert(Flatten.extract(tree) === Set(
      Flatten("a", "b"),
      Flatten("a", "b", Some("c" -> Left)),
      Flatten("b", "e"),
      Flatten("b", "e", Some("d" -> Right))
    ))
  }

  test("apply promote transformation") {
    val tree = SyntaxTree.parse("( (NP (NP (DT all) (JJ peacekeeping) (NNS forces)) (PP (IN of) (NP the UN))) )")
    assert(Promote("NP", "NP", "DT", Left)(tree).get.toString ===
      "( (NP (DT all) (NP (JJ peacekeeping) (NNS forces)) (PP (IN of) (NP the UN))) )")
    assert(Promote("NP", "NP", "NNS", Right)(tree).get.toString ===
      "( (NP (NP (DT all) (JJ peacekeeping)) (NNS forces) (PP (IN of) (NP the UN))) )")
    val newTree = Promote("NP", "PP", "IN", Left)(tree).get
    assert(newTree.toString ===
      "( (NP (NP (DT all) (JJ peacekeeping) (NNS forces)) (IN of) (PP (NP the UN))) )")
    assert(Promote("NP", "PP", "NP", Right)(newTree).get.toString ===
      "( (NP (NP (DT all) (JJ peacekeeping) (NNS forces)) (IN of) (NP the UN)) )")
    assert(Promote("NP", "PP", "NP", Right)(tree).get.toString ===
      "( (NP (NP (DT all) (JJ peacekeeping) (NNS forces)) (PP (IN of)) (NP the UN)) )")
    assert(Promote("NP", "JJ", "peacekeeping", Left)(tree).isEmpty)
    assert(Promote("NP", "JJ", "peacekeeping", Right)(tree).isEmpty)
  }

  test("extract promote transformations") {
    val tree = SyntaxTree.parse("( (NP (NP (DT all) (JJ peacekeeping) (NNS forces)) (PP (IN of) (NP the UN))) )")
    assert(Promote.extract(tree) === Set(
      Promote("NP", "NP", "DT", Left),
      Promote("NP", "NP", "NNS", Right),
      Promote("NP", "PP", "IN", Left),
      Promote("NP", "PP", "NP", Right)
    ))
  }

  test("apply demote transformation") {
    val tree = SyntaxTree.parse("( (VP (VB fly) (PP (IN to) (NP Beijing)) (PP (IN on) (NP the 2nd))) )")
    assert(Demote("VP", "PP", "VB", Right)(tree).get.toString ===
      "( (VP (PP (VB fly) (IN to) (NP Beijing)) (PP (IN on) (NP the 2nd))) )")
    assert(Demote("VP", "PP", "PP", Left)(tree).get.toString ===
      "( (VP (VB fly) (PP (IN to) (NP Beijing) (PP (IN on) (NP the 2nd)))) )")
    assert(Demote("PP", "IN", "NP", Left)(tree).isEmpty)
    assert(Demote("PP", "NP", "IN", Right)(tree).isEmpty)
    assert(Demote("NP", "the", "2nd", Left)(tree).isEmpty)
  }

  test("extract demote transformations") {
    val tree = SyntaxTree.parse("( (VP (VB fly) (PP (IN to) (NP Beijing)) (PP (IN on) (NP the 2nd))) )")
    assert(Demote.extract(tree) === Set(
      Demote("VP", "PP", "PP", Left),
      Demote("VP", "PP", "PP", Right),
      Demote("VP", "PP", "VB", Right)
    ))
  }

  test("apply transfer transformation") {
    val tree = SyntaxTree.parse("( (NP (NP (JJ serious) (NNS consequences)) (SBAR (WHNP that) (S cause losses))) )")
    val newTree = Transfer("NP", "NP", "SBAR", "WHNP", Left)(tree).get
    assert(newTree.toString ===
      "( (NP (NP (JJ serious) (NNS consequences) (WHNP that)) (SBAR (S cause losses))) )")
    assert(Transfer("NP", "NP", "SBAR", "S", Left)(newTree).get.toString ===
      "( (NP (NP (JJ serious) (NNS consequences) (WHNP that) (S cause losses))) )")
    assert(Transfer("NP", "SBAR", "NP", "NNS", Right)(tree).get.toString ===
      "( (NP (NP (JJ serious)) (SBAR (NNS consequences) (WHNP that) (S cause losses))) )")
    assert(Transfer("NP", "JJ", "NNS", "consequences", Left)(tree).isEmpty)
    assert(Transfer("NP", "NNS", "JJ", "serious", Right)(tree).isEmpty)
  }

  test("extract transfer transformations") {
    val tree = SyntaxTree.parse("( (NP (NP (JJ serious) (NNS consequences)) (SBAR (WHNP that) (S cause losses))) )")
    assert(Transfer.extract(tree) === Set(
      Transfer("NP", "NP", "SBAR", "WHNP", Left),
      Transfer("NP", "SBAR", "NP", "NNS", Right)
    ))
  }

  test("apply adopt transformation") {
    val tree = SyntaxTree.parse("( (S (NP Sabor) (ADVP (RB also)) (VP (VBD tied) (PP with Setangon))) )")
    assert(Adopt("S", "VP", "ADVP", "RB", Right)(tree).get.toString ===
      "( (S (NP Sabor) (RB+VP (RB also) (VP (VBD tied) (PP with Setangon)))) )")
    val newTree = Adopt("S", "ADVP", "VP", "VBD", Left)(tree).get
    assert(newTree.toString ===
      "( (S (NP Sabor) (ADVP+VBD (ADVP (RB also)) (VBD tied)) (VP (PP with Setangon))) )")
    assert(Adopt("S", "ADVP+VBD", "VP", "PP", Left)(newTree).isEmpty)
    assert(Adopt("VP", "VBD", "PP", "with", Left)(tree).isEmpty)
    assert(Adopt("S", "ADVP", "NP", "Sabor", Right)(tree).isEmpty)
  }

  test("extract adopt transformations") {
    val tree = SyntaxTree.parse("( (S (NP Sabor) (ADVP (RB also)) (VP (VBD tied) (PP with Setangon))) )")
    assert(Adopt.extract(tree) === Set(
      Adopt("S", "NP", "ADVP", "RB", Left),
      Adopt("S", "ADVP", "VP", "VBD", Left),
      Adopt("S", "VP", "ADVP", "RB", Right)
    ))

    val newTree = Adopt("S", "ADVP", "VP", "VBD", Left)(tree).get
    assert(Adopt.extract(newTree) === Set(
      Adopt("S", "NP", "ADVP+VBD", "ADVP", Left),
      Adopt("S", "VP", "ADVP+VBD", "VBD", Right),
      Adopt("ADVP+VBD", "VBD", "ADVP", "RB", Right)
    ))
  }
}
