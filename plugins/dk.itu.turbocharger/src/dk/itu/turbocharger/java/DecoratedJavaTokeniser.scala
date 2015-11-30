package dk.itu.turbocharger.java

import dk.itu.turbocharger.parsing.Tokeniser

object DecoratedJavaTokeniser extends Tokeniser {
  val java = Token(Partitioning.Java.ContentTypes.JAVA)
  val jslc = Token(Partitioning.Java.ContentTypes.COMMENT)
  val jmlc = Token(Partitioning.Java.ContentTypes.COMMENT)
  val jcl = Token(Partitioning.Java.ContentTypes.CHAR)
  val jsl = Token(Partitioning.Java.ContentTypes.STRING)

  val coq = Token(Partitioning.Coq.ContentTypes.COQ)
  val cc = Token(Partitioning.Coq.ContentTypes.COMMENT)
  val csl = Token(Partitioning.Coq.ContentTypes.STRING)

  import DecoratedJavaRecogniser._
  InterestingTransition(tJavaToChar, jcl, 1)
  InterestingTransition(tCharToJava, java, 0)
  InterestingTransition(tJavaToString, jsl, 1)
  InterestingTransition(tStringToJava, java, 0)
  InterestingTransition(tNCoqToChar, jcl, 1)
  InterestingTransition(tNCoqToString, jsl, 1)
  InterestingTransition(tNCommentToChar, jcl, 1)
  InterestingTransition(tNCommentToString, jsl, 1)
  InterestingTransition(tJavaToSLComment, jslc, 2)
  InterestingTransition(tSLCommentToJava, java, 0)
  InterestingTransition(tJavaToMLComment, jmlc, 2)
  InterestingTransition(tMLCommentToJava, java, 0)

  InterestingTransition(tJavaToCoq, coq, 2)
  InterestingTransition(tCoqToJava, java, 0)
  InterestingTransition(tCoqToString, csl, 1)
  InterestingTransition(tNJavaToString, csl, 1)
  InterestingTransition(tNCCommentToString, csl, 1)
  InterestingTransition(tStringToCoq, coq, 0)
  InterestingTransition(tCoqToComment, cc, 2)
  InterestingTransition(tCommentToCoq, coq, 0)

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
