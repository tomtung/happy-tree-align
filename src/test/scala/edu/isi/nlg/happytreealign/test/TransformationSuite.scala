package edu.isi.nlg.happytreealign.test

import org.scalatest.FunSuite
import edu.isi.nlg.happytreealign._
import edu.isi.nlg.happytreealign.trans._

class TransformationSuite extends FunSuite {
  test("apply articulate transformation") {
    val tree1 = SyntaxTree.parse("( (S (NP (DUMMY Other) members) (VP will (DUMMY arrive) in two groups) (. .)) )")
    assert(Articulate(parentLabel = "S", leftLabel = "NP", rightLabel = "VP").applyOption(tree1).get.toString ===
      "( (S (NP+VP (NP (DUMMY Other) members) (VP will (DUMMY arrive) in two groups)) (. .)) )")

    val tree2 = SyntaxTree.parse("( (a (a (b x) (c x) x) b c) )")
    val newTree2 = Articulate("a", "b", "c").applyOption(tree2).get
    assert(newTree2.toString ===
      "( (a (a (b+c (b x) (c x)) x) (b+c b c)) )")

    assert(Articulate("a+b", "a", "b").applyOption(newTree2).isEmpty)

    val tree3 = SyntaxTree.parse("( (a b c) )")
    assert(Articulate("a", "b", "c").applyOption(tree3).isEmpty)
  }

  test("extract articulate transformations") {
    val tree = SyntaxTree.parse("( (a (b x x x) (a b (a x)) c) )")
    assert(Articulate.extract(tree) === Set(
      Articulate("a", "b", "a"),
      Articulate("a", "a", "c")
    ))

    val newTree = Articulate(parentLabel = "a", leftLabel = "b", rightLabel = "a").applyOption(tree).get
    assert(Articulate.extract(newTree) === Set.empty)
  }

  test("apply flatten transformation") {
    val tree1 = SyntaxTree.parse("( (NP (DT the) (NNP1 China) (NML (NNP Trade) (NNP Promotion)) (NNP2 Council)) )")
    assert(Flatten("NP", "NML").applyOption(tree1).get.toString ===
      "( (NP (DT the) (NNP1 China) (NNP Trade) (NNP Promotion) (NNP2 Council)) )")

    assert(
      Flatten("NP", "NML", Some(("NNP1", Right))).applyOption(tree1).get.toString ===
        "( (NP (DT the) (NNP1 China) (NNP Trade) (NNP Promotion) (NNP2 Council)) )")
    assert(
      Flatten("NP", "NML", Some(("NNP2", Left))).applyOption(tree1).get.toString ===
        "( (NP (DT the) (NNP1 China) (NNP Trade) (NNP Promotion) (NNP2 Council)) )")

    assert(Flatten("NP", "NML", Some(("NNP1", Left))).applyOption(tree1).isEmpty)
    assert(Flatten("NP", "NML", Some(("NNP2", Right))).applyOption(tree1).isEmpty)

    val tree2 = SyntaxTree.parse("( (a (b x (b x (b x x))) c) )")
    assert(Flatten("a", "b", Some("c" -> Left)).applyOption(tree2).get.toString ===
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
    assert(Promote("NP", "NP", "DT", Left).applyOption(tree).get.toString ===
      "( (NP (DT all) (NP (JJ peacekeeping) (NNS forces)) (PP (IN of) (NP the UN))) )")
    assert(Promote("NP", "NP", "NNS", Right).applyOption(tree).get.toString ===
      "( (NP (NP (DT all) (JJ peacekeeping)) (NNS forces) (PP (IN of) (NP the UN))) )")
    val newTree = Promote("NP", "PP", "IN", Left).applyOption(tree).get
    assert(newTree.toString ===
      "( (NP (NP (DT all) (JJ peacekeeping) (NNS forces)) (IN of) (PP (NP the UN))) )")
    assert(Promote("NP", "PP", "NP", Right).applyOption(newTree).get.toString ===
      "( (NP (NP (DT all) (JJ peacekeeping) (NNS forces)) (IN of) (NP the UN)) )")
    assert(Promote("NP", "PP", "NP", Right).applyOption(tree).get.toString ===
      "( (NP (NP (DT all) (JJ peacekeeping) (NNS forces)) (PP (IN of)) (NP the UN)) )")
    assert(Promote("NP", "JJ", "peacekeeping", Left).applyOption(tree).isEmpty)
    assert(Promote("NP", "JJ", "peacekeeping", Right).applyOption(tree).isEmpty)
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
    assert(Demote("VP", "PP", "VB", Right).applyOption(tree).get.toString ===
      "( (VP (PP (VB fly) (IN to) (NP Beijing)) (PP (IN on) (NP the 2nd))) )")
    assert(Demote("VP", "PP", "PP", Left).applyOption(tree).get.toString ===
      "( (VP (VB fly) (PP (IN to) (NP Beijing) (PP (IN on) (NP the 2nd)))) )")
    assert(Demote("PP", "IN", "NP", Left).applyOption(tree).isEmpty)
    assert(Demote("PP", "NP", "IN", Right).applyOption(tree).isEmpty)
    assert(Demote("NP", "the", "2nd", Left).applyOption(tree).isEmpty)
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
    val newTree = Transfer("NP", "NP", "SBAR", "WHNP", Left).applyOption(tree).get
    assert(newTree.toString ===
      "( (NP (NP (JJ serious) (NNS consequences) (WHNP that)) (SBAR (S cause losses))) )")
    assert(Transfer("NP", "NP", "SBAR", "S", Left).applyOption(newTree).get.toString ===
      "( (NP (NP (JJ serious) (NNS consequences) (WHNP that) (S cause losses))) )")
    assert(Transfer("NP", "SBAR", "NP", "NNS", Right).applyOption(tree).get.toString ===
      "( (NP (NP (JJ serious)) (SBAR (NNS consequences) (WHNP that) (S cause losses))) )")
    assert(Transfer("NP", "JJ", "NNS", "consequences", Left).applyOption(tree).isEmpty)
    assert(Transfer("NP", "NNS", "JJ", "serious", Right).applyOption(tree).isEmpty)
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
    assert(Adopt("S", "VP", "ADVP", "RB", Right).applyOption(tree).get.toString ===
      "( (S (NP Sabor) (RB+VP (RB also) (VP (VBD tied) (PP with Setangon)))) )")
    val newTree = Adopt("S", "ADVP", "VP", "VBD", Left).applyOption(tree).get
    assert(newTree.toString ===
      "( (S (NP Sabor) (ADVP+VBD (ADVP (RB also)) (VBD tied)) (VP (PP with Setangon))) )")
    assert(Adopt("S", "ADVP+VBD", "VP", "PP", Left).applyOption(newTree).isEmpty)
    assert(Adopt("VP", "VBD", "PP", "with", Left).applyOption(tree).isEmpty)
    assert(Adopt("S", "ADVP", "NP", "Sabor", Right).applyOption(tree).isEmpty)
  }

  test("extract adopt transformations") {
    val tree = SyntaxTree.parse("( (S (NP Sabor) (ADVP (RB also)) (VP (VBD tied) (PP with Setangon))) )")
    assert(Adopt.extract(tree) === Set(
      Adopt("S", "NP", "ADVP", "RB", Left),
      Adopt("S", "ADVP", "VP", "VBD", Left),
      Adopt("S", "VP", "ADVP", "RB", Right)
    ))

    val newTree = Adopt("S", "ADVP", "VP", "VBD", Left).applyOption(tree).get
    assert(Adopt.extract(newTree) === Set(
      Adopt("S", "NP", "ADVP+VBD", "ADVP", Left),
      Adopt("S", "VP", "ADVP+VBD", "VBD", Right),
      Adopt("ADVP+VBD", "VBD", "ADVP", "RB", Right)
    ))
  }
}
