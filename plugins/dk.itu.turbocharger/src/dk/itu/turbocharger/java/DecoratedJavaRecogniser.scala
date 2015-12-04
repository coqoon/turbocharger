package dk.itu.turbocharger.java

import dk.itu.turbocharger.parsing.PushdownAutomaton

object DecoratedJavaRecogniser extends PushdownAutomaton[Char] {
  import PushdownAutomaton.Element

  object States {
    import PushdownAutomaton.State

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
  import Actions._
  DefaultTransition(java, java)

  val tJavaToChar = BasicTransition(java, '\'', javaChar)
  DefaultTransition(javaChar, javaChar)
  BasicTransition(javaChar, '\\', javaCharEscape)
  val tCharToJava = BasicTransition(javaChar, '\'', java)
  DefaultTransition(javaCharEscape, javaChar)

  val tJavaToString = BasicTransition(java, '\"', javaString)
  DefaultTransition(javaString, javaString)
  BasicTransition(javaString, '\\', javaStringEscape)
  val tStringToJava = BasicTransition(javaString, '\"', java)
  DefaultTransition(javaStringEscape, javaString)

  BasicTransition(java, '/', nearlyJavaComment)
  DefaultTransition(nearlyJavaComment, java)
  val tNCommentToChar = BasicTransition(nearlyJavaComment, '\'', javaChar)
  val tNCommentToString = BasicTransition(nearlyJavaComment, '\"', javaString)

  val tJavaToSLComment =
    BasicTransition(nearlyJavaComment, '/', javaSingleLineComment)
  DefaultTransition(javaSingleLineComment, javaSingleLineComment)
  val tSLCommentToJava = BasicTransition(javaSingleLineComment, '\n', java)

  val tJavaToMLComment =
  BasicTransition(nearlyJavaComment, '*', javaMultiLineComment)
  DefaultTransition(javaMultiLineComment, javaMultiLineComment)
  BasicTransition(javaMultiLineComment, '*', nearlyOutOfJavaComment)
  DefaultTransition(nearlyOutOfJavaComment, javaMultiLineComment)
  BasicTransition(nearlyOutOfJavaComment, '*', nearlyOutOfJavaComment)
  val tMLCommentToJava = BasicTransition(nearlyOutOfJavaComment, '/', java)

  BasicTransition(java, '<', nearlyCoq)
  val tNCoqToString = BasicTransition(nearlyCoq, '"', coqString)
  val tNCoqToChar = BasicTransition(nearlyCoq, '\'', javaChar)
  BasicTransition(nearlyCoq, '/', nearlyJavaComment)
  DefaultTransition(nearlyCoq, java)
  val tJavaToCoq = BasicTransition(nearlyCoq, '%', coq)

  BasicTransition(coq, '%', nearlyJava)
  val tNJavaToString = BasicTransition(nearlyJava, '"', coqString)
  BasicTransition(nearlyJava, '(', nearlyCoqComment)
  DefaultTransition(nearlyJava, coq)
  val tCoqToJava = BasicTransition(nearlyJava, '>', java)

  DefaultTransition(coq, coq)

  val tCoqToString = BasicTransition(coq, '"', coqString)
  DefaultTransition(coqString, coqString)
  BasicTransition(coqStringEscape, '\\', coqStringEscape)
  val tStringToCoq = BasicTransition(coqString, '\"', coq)
  DefaultTransition(coqStringEscape, coqString)

  BasicTransition(coq, '(', nearlyCoqComment)
  val tNCCommentToString = BasicTransition(nearlyCoqComment, '"', coqString)
  DefaultTransition(nearlyCoqComment, coq)
  BasicTransition(nearlyCoqComment, '(', nearlyCoqComment)
  val tCoqToComment = BasicTransition(nearlyCoqComment, '*', coqComment)

  DefaultTransition(coqComment, coqComment)
  val commentElement = Element("COM!")

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
  val tCommentToCoq = BasicTransition(nearlyOutOfCoqComment, ')', coq)
}
