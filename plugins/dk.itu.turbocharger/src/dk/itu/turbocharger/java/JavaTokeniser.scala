package dk.itu.turbocharger.java

import dk.itu.turbocharger.parsing.{Tokeniser, PushdownAutomaton}

object JavaRecogniser extends PushdownAutomaton[Char] {
  object States {
    import PushdownAutomaton.State
    val base = "JavaTokeniser:"

    val java = State(s"${base}Java")

    val javaString = State(s"${base}Java string")
    val javaStringEscape = State(s"${base}Java string escape")

    val javaChar = State(s"${base}Java character")
    val javaCharEscape = State(s"${base}Java character escape")

    val nearlyJavaComment = State(s"${base}Nearly Java comment")
    val javaSingleLineComment = State(s"${base}Java single-line comment")
    val javaMultiLineComment = State(s"${base}Java multi-line comment")
    val nearlyOutOfJavaComment = State(s"${base}Nearly out of Java comment")
  }

  import States._
  import Actions._

  DefaultTransition(java, java)

  BasicTransition(java, '\'', javaChar)
  DefaultTransition(javaChar, javaChar)
  BasicTransition(javaChar, '\\', javaCharEscape)
  BasicTransition(javaChar, '\'', java)
  DefaultTransition(javaCharEscape, javaChar)

  BasicTransition(java, '\"', javaString)
  DefaultTransition(javaString, javaString)
  BasicTransition(javaString, '\\', javaStringEscape)
  BasicTransition(javaString, '\"', java)
  DefaultTransition(javaStringEscape, javaString)

  BasicTransition(java, '/', nearlyJavaComment)
  DefaultTransition(nearlyJavaComment, java)
  BasicTransition(nearlyJavaComment, '\'', javaChar)
  BasicTransition(nearlyJavaComment, '\"', javaString)

  BasicTransition(nearlyJavaComment, '/', javaSingleLineComment)
  DefaultTransition(javaSingleLineComment, javaSingleLineComment)
  BasicTransition(javaSingleLineComment, '\n', java)

  BasicTransition(nearlyJavaComment, '*', javaMultiLineComment)
  DefaultTransition(javaMultiLineComment, javaMultiLineComment)
  BasicTransition(javaMultiLineComment, '*', nearlyOutOfJavaComment)
  DefaultTransition(nearlyOutOfJavaComment, javaMultiLineComment)
  BasicTransition(nearlyOutOfJavaComment, '*', nearlyOutOfJavaComment)
  BasicTransition(nearlyOutOfJavaComment, '/', java)
}

object JavaTokeniser extends Tokeniser(JavaRecogniser) {
  import PushdownAutomaton.{State, Transition}
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