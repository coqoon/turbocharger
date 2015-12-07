package dk.itu.turbocharger.java

import dk.itu.turbocharger.parsing.Tokeniser
import dk.itu.turbocharger.parsing.PushdownAutomaton.Transition

object DecoratedJavaTokeniser extends Tokeniser {
  val java = Token(Partitioning.Java.ContentTypes.JAVA)
  val jslc = Token(Partitioning.Java.ContentTypes.COMMENT)
  val jmlc = Token(Partitioning.Java.ContentTypes.COMMENT)
  val jcl = Token(Partitioning.Java.ContentTypes.CHAR)
  val jsl = Token(Partitioning.Java.ContentTypes.STRING)

  val coq = Token(Partitioning.Coq.ContentTypes.COQ)
  val cc = Token(Partitioning.Coq.ContentTypes.COMMENT)
  val csl = Token(Partitioning.Coq.ContentTypes.STRING)

  import DecoratedJavaRecogniser.States
  TransitionInspector {
    case Transition(_, _, _, _, States.javaChar) => Some((jcl, 1))
    case Transition(_, _, _, _, States.javaString) => Some((jsl, 1))
    case Transition(_, _, _, _, States.javaSingleLineComment) =>
      Some((jslc, 2))
    case Transition(_, _, _, _, States.javaMultiLineComment) => Some((jmlc, 2))

    case Transition(States.nearlyCoq, _, _, _, States.coq) => Some((coq, 2))
    case Transition(States.nearlyJava, _, _, _, States.java) => Some((java, 0))

    case Transition(_, _, _, _, States.coqString) => Some((csl, 1))
    case Transition(_, _, _, _, States.coqComment) => Some((cc, 2))

    case Transition(c, _, _, _, States.coq)
        if c != States.coq =>
      Some((coq, 0))
    case Transition(j, _, _, _, States.java)
        if j != States.java =>
      Some((java, 0))

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
    tokens(java, DJR.Execution(DJR.States.java, Seq()), input)
  }
}
