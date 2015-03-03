package dk.itu.turbocharger.java

object Partitioning {
  final val ID = "d.i.c.t.j"
  object Java {
    object ContentTypes {
      final val JAVA = s"${ID}:java"
      final val CHAR = s"${JAVA}:char"
      final val STRING = s"${JAVA}:string"
      final val COMMENT = s"${JAVA}:comment"
    }
  }
  object Coq {
    object ContentTypes {
      final val COQ = s"${ID}:coq"
      final val STRING = s"${COQ}:string"
      final val COMMENT = s"${COQ}:comment"
    }
  }
  import Coq.{ContentTypes => CCT}
  import Java.{ContentTypes => JCT}
  final val TYPES =
    Array(JCT.JAVA, JCT.CHAR, JCT.STRING, JCT.COMMENT,
          CCT.COQ, CCT.STRING, CCT.COMMENT)
}