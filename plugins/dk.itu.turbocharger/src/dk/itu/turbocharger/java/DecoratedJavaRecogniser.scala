package dk.itu.turbocharger.java

import dk.itu.turbocharger.parsing.PushdownAutomaton

object DecoratedJavaRecogniser extends PushdownAutomaton[Char] {
  object States {
    val java = State("Java")
    val nearlyCoq = State("Nearly Coq")
    val coq = State("Coq")
    val nearlyJava = State("Nearly Java")

    val javaString = State("Java string")
    val javaStringEscape = State("Java string escape")

    val javaChar = State("Java character")
    val javaCharEscape = State("Java character escape")

    val nearlyJavaComment = State("Nearly Java comment")
    val javaSingleLineComment = State("Java single-line comment")
    val javaMultiLineComment = State("Java multi-line comment")
    val nearlyOutOfJavaComment = State("Nearly out of Java comment")

    val nearlyCoqComment = State("Nearly Coq comment")
    val coqComment = State("Coq comment")
    val nearlyNestedCoqComment = State("Nearly nested Coq comment")
    val nearlyOutOfCoqComment = State("Nearly out of Coq comment")

    val coqString = State("Coq string")
    val coqStringEscape = State("Coq string escape")
  }

  import States._
  java.DefaultTransition(java)

  val tJavaToChar = java.BasicTransition('\'', javaChar)
  javaChar.DefaultTransition(javaChar)
  javaChar.BasicTransition('\\', javaCharEscape)
  val tCharToJava = javaChar.BasicTransition('\'', java)
  javaCharEscape.DefaultTransition(javaChar)

  val tJavaToString = java.BasicTransition('\"', javaString)
  javaString.DefaultTransition(javaString)
  javaString.BasicTransition('\\', javaStringEscape)
  val tStringToJava = javaString.BasicTransition('\"', java)
  javaStringEscape.DefaultTransition(javaString)

  java.BasicTransition('/', nearlyJavaComment)
  nearlyJavaComment.DefaultTransition(java)
  val tNCommentToChar = nearlyJavaComment.BasicTransition('\'', javaChar)
  val tNCommentToString = nearlyJavaComment.BasicTransition('\"', javaString)

  val tJavaToSLComment =
    nearlyJavaComment.BasicTransition('/', javaSingleLineComment)
  javaSingleLineComment.DefaultTransition(javaSingleLineComment)
  val tSLCommentToJava = javaSingleLineComment.BasicTransition('\n', java)

  val tJavaToMLComment =
    nearlyJavaComment.BasicTransition('*', javaMultiLineComment)
  javaMultiLineComment.DefaultTransition(javaMultiLineComment)
  javaMultiLineComment.BasicTransition('*', nearlyOutOfJavaComment)
  nearlyOutOfJavaComment.DefaultTransition(javaMultiLineComment)
  nearlyOutOfJavaComment.BasicTransition('*', nearlyOutOfJavaComment)
  val tMLCommentToJava = nearlyOutOfJavaComment.BasicTransition('/', java)

  /* XXX: nearlyCoq states don't support transitions to other Java/Coq ones */
  java.BasicTransition('<', nearlyCoq)
  nearlyCoq.DefaultTransition(java)
  val tJavaToCoq = nearlyCoq.BasicTransition('%', coq)

  coq.BasicTransition('%', nearlyJava)
  nearlyJava.DefaultTransition(coq)
  val tCoqToJava = nearlyJava.BasicTransition('>', java)

  coq.DefaultTransition(coq)

  val tCoqToString = coq.BasicTransition('\"', coqString)
  coqString.DefaultTransition(coqString)
  coqStringEscape.BasicTransition('\\', coqStringEscape)
  val tStringToCoq = coqString.BasicTransition('\"', coq)
  coqStringEscape.DefaultTransition(coqString)

  coq.BasicTransition('(', nearlyCoqComment)
  nearlyCoqComment.DefaultTransition(coq)
  nearlyCoqComment.BasicTransition('(', nearlyCoqComment)
  val tCoqToComment = nearlyCoqComment.BasicTransition('*', coqComment)

  coqComment.DefaultTransition(coqComment)
  val commentElement = Element("COM!")

  coqComment.BasicTransition('(', nearlyNestedCoqComment)
  nearlyNestedCoqComment.DefaultTransition(coqComment)
  nearlyNestedCoqComment.BasicTransition('(', nearlyNestedCoqComment)
  nearlyNestedCoqComment.Transition(
      None, Some('*'), Some(commentElement), coqComment)

  coqComment.BasicTransition('*', nearlyOutOfCoqComment)
  nearlyOutOfCoqComment.DefaultTransition(coqComment)
  nearlyOutOfCoqComment.BasicTransition('*', nearlyOutOfCoqComment)
  nearlyOutOfCoqComment.Transition(
      Some(commentElement), Some(')'), None, coqComment)
  val tCommentToCoq = nearlyOutOfCoqComment.BasicTransition(')', coq)
}
