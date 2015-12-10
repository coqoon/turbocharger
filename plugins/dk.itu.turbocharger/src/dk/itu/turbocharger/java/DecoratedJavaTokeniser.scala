package dk.itu.turbocharger.java

import dk.itu.turbocharger.parsing.Tokeniser
import dk.itu.turbocharger.parsing.PushdownAutomaton.Transition

object DecoratedJavaTokeniser extends Tokeniser {
  import DecoratedJavaRecogniser.States

  TransitionInspector {
    case Transition(f, _, _, _, t @ States.javaChar) if f != t =>
      Some((t, 1))
    case Transition(f, _, _, _, t @ States.javaString) if f != t =>
      Some((t, 1))
    case Transition(f, _, _, _, t @ States.javaSingleLineComment) if f != t =>
      Some((t, 2))
    case Transition(f, _, _, _, t @ States.javaMultiLineComment) if f != t =>
      Some((t, 2))

    case Transition(States.nearlyCoq, _, _, _, t @ States.coq) =>
      Some((t, 2))
    case Transition(States.nearlyJava, _, _, _, t @ States.java) =>
      Some((t, 0))

    case Transition(f, _, _, _, t @ States.coqString) if f != t =>
      Some((t, 1))
    case Transition(f, _, _, _, t @ States.coqComment) if f != t =>
      Some((t, 2))

    case Transition(f, _, _, _, t @ States.coq) if f != t =>
      Some((t, 0))
    case Transition(f, _, _, _, t @ States.java) if f != t =>
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

  def tokenise(input : String) = {
    import dk.itu.turbocharger.java.{DecoratedJavaRecogniser => DJR}
    tokens(DJR.Execution(States.java, Seq()), input)
  }
}
