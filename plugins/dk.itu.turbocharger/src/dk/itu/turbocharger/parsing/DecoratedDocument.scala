package dk.itu.turbocharger.parsing

class DecoratedDocument(tokens : DecoratedDocument.Tokens) {
  import DecoratedDocument._

  class View(prefix : String) {
    lazy val tokens = DecoratedDocument.this.getTokens.filter(contains)
    def getTokens() = tokens
    def get() = getTokens.flatMap(_._2).mkString
    def contains(token : Token) = token._1.label.startsWith(prefix)
    def toDocumentOffset(offset : Int) : Option[Int] = {
      var viewPos = 0
      for ((realPos, q @ (t, s)) <-
          DecoratedDocument.this.getTokensWithPositions) {
        if (contains(q)) {
          val viewEnd = (viewPos + s.length)
          if (offset >= viewPos && offset < viewEnd) {
            return Some(realPos + (offset - viewPos))
          } else viewPos = viewEnd
        }
      }
      None
    }
    def toDocumentRegions(viewRegion : Region) : Seq[Region] = {
      val r = (toDocumentOffset(viewRegion.start),
          toDocumentOffset(viewRegion.end)) match {
        case (Some(ds), Some(de)) =>
          val documentRegion = Region(ds, length = de - ds)
          var result = Seq[Region]()
          for ((pos, q @ (t, s)) <-
              DecoratedDocument.this.getTokensWithPositions) {
            val tokenRegion = Region(pos, length = s.length)
            if (contains(q))
              documentRegion.intersection(tokenRegion).foreach(result :+= _)
          }
          result
        case _ =>
          Seq()
      }
      r
    }
    def toSingleDocumentRegion(viewRegion : Region) : Region =
      toDocumentRegions(viewRegion).reduce((r1, r2) => r1.union(r2))
  }

  /* Returns all the tokens in this document. */
  def getTokens() : Tokens = tokens
  /* If there are any tokens completely contained within @viewRegion, then
   * return them, along with the document-relative offset of the first one. */
  def getTokens(region : Region) : Option[(Int, Tokens)] = {
    var initialPosition : Option[Int] = None
    var result : Tokens = Seq()
    for ((pos, q @ (t, s)) <- getTokensWithPositions) {
      val tokenRegion = Region(pos, length = s.length)
      if (region.contains(tokenRegion)) {
        if (initialPosition == None)
          initialPosition = Some(pos)
        result :+= q
      }
    }
    initialPosition.map(ip => (ip, result))
  }

  def getTokensWithPositions() : Stream[(Int, Token)] = {
    var pos = 0
    tokens.toStream.map(t => try ((pos, t)) finally pos += t._2.length)
  }
}
object DecoratedDocument {
  type Token = (Tokeniser#Token, String)
  type Tokens = Seq[Token]
  case class Region(
      start : Int, length : Int) extends org.eclipse.jface.text.IRegion {
    lazy val end = (start + length)
    assert(start >= 0 && length >= 0)

    override def getLength() = length
    override def getOffset() = start

    /* Returns the smallest Region containing both this region and @r. */
    def union(r : Region) = {
      val newStart = Math.min(start, r.start)
      val newLength = Math.max(end, r.end) - newStart
      Region(newStart, length = newLength)
    }
    /* Returns a Region corresponding to the overlap between this region and
     * @r, if such an overlap exists. */
    def intersection(r : Region) : Option[Region] =
      if (r.end < start || r.start >= end) {
        None
      } else {
        val newStart = Math.max(start, r.start)
        val newLength = Math.min(end, r.end) - newStart
        Some(Region(newStart, length = newLength))
      }
    def contains(r : Region) = (union(r) == this)
  }
}