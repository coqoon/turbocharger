package dk.itu.turbocharger.java

import dk.itu.turbocharger.parsing.{Tokeniser, DecoratedDocument}

class DecoratedJavaDocument(tokens : Seq[(Tokeniser#Token, String)])
    extends DecoratedDocument(tokens) {
  def getCoqView() = new TypedView(Partitioning.Coq.ContentTypes.COQ)
  def getJavaView() = new TypedView(Partitioning.Java.ContentTypes.JAVA)
}
