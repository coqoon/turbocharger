package dk.itu.turbocharger.java

import dk.itu.turbocharger.parsing.{Tokeniser, DecoratedDocument}

class DecoratedJavaDocument(tokens : Seq[(Tokeniser#Token, String)])
    extends DecoratedDocument(tokens) {
  def getCoqView() = new View(Partitioning.Coq.ContentTypes.COQ)
  def getJavaView() = new View(Partitioning.Java.ContentTypes.JAVA)
}
