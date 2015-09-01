package dk.itu.turbocharger.coq

object PIDEDiff {
  def longest[A](a : List[A], b : List[A]) =
    if (a.length >= b.length) a else b
  def longestCommonSubsequence[A](compare : (A, A) => Boolean)(
      a : List[A], b : List[A]) : List[A] =
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

  def makeZanyDiff[A](
      a : List[A], b : List[A]) : List[(Option[A], Option[A])] =
    makeZanyDiff((a : A, b : A) => a == b)(a, b)
  def makeZanyDiff[A](compare : (A, A) => Boolean)(
      a : List[A], b : List[A]) : List[(Option[A], Option[A])] = {
    val lcs = longestCommonSubsequence(compare)(a, b)

    var result : List[(Option[A], Option[A])] = List()

    var lastHook : Option[A] = None
    var sr : Seq[A] = a
    var lcsr = lcs
    while (sr != Nil) (sr, lcsr) match {
      case (sh :: srest, lcsh :: lcsrest)
          if compare(sh, lcsh) =>
        lastHook = Some(sh)
        sr = srest
        lcsr = lcsrest
      case (sh :: srest, _) =>
        result :+= (lastHook, None)
        sr = srest
    }

    lastHook = None
    sr = b
    lcsr = lcs
    while (sr != Nil) (sr, lcsr) match {
      case (sh :: srest, lcsh :: lcsrest)
          if compare(sh, lcsh) =>
        lastHook = Some(sh)
        sr = srest
        lcsr = lcsrest
      case (sh :: srest, _) =>
        result :+= (lastHook, Some(sh))
        lastHook = Some(sh)
        sr = srest
    }

    result
  }
}