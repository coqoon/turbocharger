package dk.itu.turbocharger.coq

class Differ[A] {
  private var cache : Map[(List[A], List[A]), List[A]] = Map()

  def longest(a : List[A], b : List[A]) =
    if (a.length >= b.length) a else b
  def longestCommonSubsequence(compare : (A, A) => Boolean)(
      a : List[A], b : List[A]) : List[A] =
    cache.get(a, b) match {
      case Some(lcs) =>
        lcs
      case None =>
        val lcs =
          (a, b) match {
            case (Nil, _) => List()
            case (_, Nil) => List()
            case (ae :: ra, be :: rb) if compare(ae, be) =>
              ae :: longestCommonSubsequence(compare)(ra, rb)
            case (ae :: ra, be :: rb) =>
              longest(
                  longestCommonSubsequence(compare)(a, rb),
                  longestCommonSubsequence(compare)(ra, b))
          }
        cache += (a, b) -> lcs
        lcs
    }

  def diff[B](a : List[A], b : List[A],
      add : A => B, remain : A => Unit, remove : A => B) = {
    val lcs = longestCommonSubsequence((a, b) => a == b)(a, b)
    var (o, c, n) = (a, lcs, b)
    var edits = List[B]()
    while (o != Nil || n != Nil) {
      (o, c, n) match {
        case (oh :: or, c, _)
            if !c.headOption.contains(oh) =>
          edits :+= remove(oh)
          o = or
        case (_, c, nh :: nr)
            if !c.headOption.contains(nh) =>
          edits :+= add(nh)
          n = nr
        case (oh :: or, ch :: cr, nh :: nr)
            if oh == ch && ch == nh =>
          remain(ch)
          o = or
          c = cr
          n = nr
      }
    }
    edits
  }
}

class PIDEDiff {
  private val differ = new Differ[String]
  private var last : List[String] = List()

  def makeEdits(a : List[String]) = {
    import isabelle.Text.Edit
    var eo = 0
    def _add(a : String) = try Edit.insert(eo, a) finally eo += a.length
    def _remain(a : String) = eo += a.length
    def _remove(a : String) = Edit.remove(eo, a)
    try {
      differ.diff(last, a, _add, _remain, _remove)
    } finally last = a
  }
}