package dk.itu.turbocharger.coq

import dk.itu.coqoon.ui.text.{Tokeniser, PushdownAutomaton}

object CoqRecogniser extends PushdownAutomaton[Char] {
  import PushdownAutomaton.Element

  object States {
    import PushdownAutomaton.State
    val base = "CoqRecogniser:"

    val coq = State(s"${base}Coq")
    val coqComment = State(s"${base}Coq comment")
    val coqString = State(s"${base}Coq string")

    val nearlyCoqComment = State(s"${base}Nearly Coq comment")
    val nearlyNestedCoqComment = State(s"${base}Nearly nested Coq comment")
    val nearlyOutOfCoqComment = State(s"${base}Nearly out of Coq comment")

    val coqStringEscape = State(s"${base}Coq string escape")
  }

  import States._
  import Actions._

  DefaultTransition(coq, coq)

  BasicTransition(coq, '"', coqString)
  DefaultTransition(coqString, coqString)
  BasicTransition(coqStringEscape, '\\', coqStringEscape)
  BasicTransition(coqString, '\"', coq)
  DefaultTransition(coqStringEscape, coqString)

  BasicTransition(coq, '(', nearlyCoqComment)
  BasicTransition(nearlyCoqComment, '"', coqString)
  DefaultTransition(nearlyCoqComment, coq)
  BasicTransition(nearlyCoqComment, '(', nearlyCoqComment)
  BasicTransition(nearlyCoqComment, '*', coqComment)

  DefaultTransition(coqComment, coqComment)
  val commentElement = Element("Coq comment")

  BasicTransition(coqComment, '(', nearlyNestedCoqComment)
  DefaultTransition(nearlyNestedCoqComment, coqComment)
  BasicTransition(nearlyNestedCoqComment, '(', nearlyNestedCoqComment)
  Transition(nearlyNestedCoqComment,
      None, Some('*'), Some(commentElement), coqComment)

  BasicTransition(coqComment, '*', nearlyOutOfCoqComment)
  DefaultTransition(nearlyOutOfCoqComment, coqComment)
  BasicTransition(nearlyOutOfCoqComment, '*', nearlyOutOfCoqComment)
  Transition(nearlyOutOfCoqComment,
      Some(commentElement), Some(')'), None, coqComment)
  BasicTransition(nearlyOutOfCoqComment, ')', coq)
}

object CoqTokeniser extends Tokeniser(CoqRecogniser) {
  import PushdownAutomaton.{State, Transition}
  import CoqRecogniser.{States => Coq}

  def coqInspector(t : Transition[Char]) : Option[(State, Int)] = t match {
    case Transition(f, _, _, _, t @ Coq.coqString) if f != t =>
      Some((t, 1))
    case Transition(f, _, _, _, t @ Coq.coqComment) if f != t =>
      Some((t, 2))

    case Transition(f, _, _, _, t @ Coq.coq) if f != t =>
      Some((t, 0))

    case _ =>
      None
  }

  TransitionInspector(coqInspector)
}