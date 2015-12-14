package dk.itu.turbocharger.java

import dk.itu.turbocharger.parsing.Tokeniser
import dk.itu.turbocharger.parsing.PushdownAutomaton.{State, Transition}

object JavaTokeniser extends Tokeniser {
  import JavaRecogniser.{States => Java}

  def javaInspector(t : Transition[Char]) : Option[(State, Int)] = t match {
    case Transition(f, _, _, _, t @ Java.javaChar) if f != t =>
      Some((t, 1))
    case Transition(f, _, _, _, t @ Java.javaString) if f != t =>
      Some((t, 1))
    case Transition(f, _, _, _, t @ Java.javaSingleLineComment)
        if f != t =>
      Some((t, 2))
    case Transition(f, _, _, _, t @ Java.javaMultiLineComment)
        if f != t =>
      Some((t, 2))

    case Transition(f, _, _, _, t @ Java.java) if f != t =>
      Some((t, 0))

    case _ =>
      None
  }

  TransitionInspector(javaInspector)
}

object CoqTokeniser extends Tokeniser {
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

object DecoratedJavaTokeniser extends Tokeniser {
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
    tokens(DecoratedJavaRecogniser.Execution(start, Seq()), input)
}
