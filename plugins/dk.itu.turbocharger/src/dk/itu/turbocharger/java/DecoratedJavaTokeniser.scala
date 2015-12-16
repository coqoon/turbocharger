package dk.itu.turbocharger.java

import dk.itu.coqoon.ui.text.Tokeniser
import dk.itu.coqoon.ui.text.PushdownAutomaton.{State, Transition}

object DecoratedJavaTokeniser extends Tokeniser(DecoratedJavaRecogniser) {
  import dk.itu.coqoon.ui.text.coq.{CoqTokeniser, CoqRecogniser}
  import CoqRecogniser.{States => Coq}
  import JavaRecogniser.{States => Java}
  import DecoratedJavaRecogniser.{States => DJ}

  TransitionInspector(CoqTokeniser.coqInspector)
  TransitionInspector(JavaTokeniser.javaInspector)
  TransitionInspector.prepend {
    case Transition(DJ.nearlyCoq, _, _, _, t @ Coq.coq) =>
      Some((t, 2))
    case Transition(DJ.nearlyJava, _, _, _, t @ Java.java) =>
      Some((t, 0))
    case _ => None
  }

  def main(args : Array[String]) = {
    var line : Option[String] = None
    do {
      line.foreach(tokenise(_).foreach(println))
      line = Some(readLine.trim).filter(_.length != 0)
    } while (line != None)
  }

  def tokenise(input : String, start : State = Java.java) =
    tokens(start, input)
}
