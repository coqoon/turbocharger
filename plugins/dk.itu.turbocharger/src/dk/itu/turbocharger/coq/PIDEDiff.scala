package dk.itu.turbocharger.coq

object PIDEDiff {
  def longest[A](a : Seq[A], b : Seq[A]) =
    if (a.length >= b.length) a else b
  def longestCommonSubsequence[A](compare : (A, A) => Boolean)(
      a : Seq[A], b : Seq[A]) : Seq[A] =
    (a, b) match {
      case (Seq(), _) => Seq()
      case (_, Seq()) => Seq()
      case (ae +: ra, be +: rb) if compare(ae, be) =>
        ae +: longestCommonSubsequence(compare)(ra, rb)
      case (ae +: ra, be +: rb) =>
        longest(
            longestCommonSubsequence(compare)(a, rb),
            longestCommonSubsequence(compare)(ra, b))
    }

  def makeZanyDiff[A](a : Seq[A], b : Seq[A]) : Seq[(Option[A], Option[A])] =
    makeZanyDiff((a : A, b : A) => a == b)(a, b)
  def makeZanyDiff[A](compare : (A, A) => Boolean)(
      a : Seq[A], b : Seq[A]) : Seq[(Option[A], Option[A])] = {
    val lcs = longestCommonSubsequence(compare)(a, b)

    var result : Seq[(Option[A], Option[A])] = Seq()

    var lastHook : Option[A] = None
    var sr : Seq[A] = a
    var lcsr = lcs
    while (sr != Nil) (sr, lcsr) match {
      case (sh +: srest, lcsh +: lcsrest)
          if compare(sh, lcsh) =>
        lastHook = Some(sh)
        sr = srest
        lcsr = lcsrest
      case (sh +: srest, _) =>
        result :+= (lastHook, None)
        sr = srest
    }

    lastHook = None
    sr = b
    lcsr = lcs
    while (sr != Nil) (sr, lcsr) match {
      case (sh +: srest, lcsh +: lcsrest)
          if compare(sh, lcsh) =>
        lastHook = Some(sh)
        sr = srest
        lcsr = lcsrest
      case (sh +: srest, _) =>
        result :+= (lastHook, Some(sh))
        lastHook = Some(sh)
        sr = srest
    }

    result
  }
}