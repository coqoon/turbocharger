package dk.itu.turbocharger.java

import dk.itu.coqoon.ui.text.PushdownAutomaton
import dk.itu.turbocharger.coq.CoqRecogniser

object DecoratedJavaRecogniser
    extends PushdownAutomaton[Char](CoqRecogniser, JavaRecogniser) {
  object States {
    import PushdownAutomaton.State

    val nearlyCoq = State("Nearly Coq")
    val nearlyJava = State("Nearly Java")
  }

  import States._
  import CoqRecogniser.States._
  import JavaRecogniser.States._
  import Actions._

  BasicTransition(java, '<', nearlyCoq)
  BasicTransition(nearlyCoq, '"', coqString)
  BasicTransition(nearlyCoq, '\'', javaChar)
  BasicTransition(nearlyCoq, '/', nearlyJavaComment)
  DefaultTransition(nearlyCoq, java)
  BasicTransition(nearlyCoq, '%', coq)

  BasicTransition(coq, '%', nearlyJava)
  BasicTransition(nearlyJava, '"', coqString)
  BasicTransition(nearlyJava, '(', nearlyCoqComment)
  DefaultTransition(nearlyJava, coq)
  BasicTransition(nearlyJava, '>', java)
}
