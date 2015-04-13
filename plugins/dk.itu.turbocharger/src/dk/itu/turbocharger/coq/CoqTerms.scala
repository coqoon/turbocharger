package dk.itu.turbocharger.coq

trait CoqThing {
  override def toString : String
}

trait CoqCommand extends CoqThing

case class Definition(name : String, value : CoqTerm) extends CoqCommand {
  override def toString = s"""Definition ${name} := ${value}."""
}

trait CoqTerm extends CoqThing

case class IdentifierTerm(a : String) extends CoqTerm {
  override def toString = s"""$a"""
}

case class StringTerm(a : String) extends CoqTerm {
  override def toString = s""""$a""""
}

case class BooleanTerm(a : Boolean) extends CoqTerm {
  override def toString = s"$a"
}

case class NatTerm(a : Int) extends CoqTerm {
  assert(a >= 0)
  override def toString = s"$a"
}

case class ZTerm(a : Int) extends CoqTerm {
  override def toString =
    if (a < 0) {
      s"($a)%Z"
    } else s"$a%Z"
}

case class ListTerm(a : List[_ <: CoqTerm]) extends CoqTerm {
  override def toString = stringise(a)

  private def stringise(a : List[_ <: CoqTerm]) : String = a match {
    case a :: b =>
      s"(${a} :: ${stringise(b)})"
    case Nil =>
      "nil"
  }
}

case class TupleTerm(a : Product) extends CoqTerm {
  override def toString = a.productIterator.mkString("(", ", ", ")")
}

trait ConstructorInvocation extends CoqTerm
case class ConstructorInvocation0(
    constructor : String) extends ConstructorInvocation {
  override def toString =
    Seq(constructor).mkString(" ")
}
case class ConstructorInvocation1(constructor : String,
    a : CoqTerm) extends ConstructorInvocation {
  override def toString =
    Seq(constructor, a).mkString("(", " ", ")")
}
case class ConstructorInvocation2(constructor : String,
    a : CoqTerm, b : CoqTerm) extends ConstructorInvocation {
  override def toString =
    Seq(constructor, a, b).mkString("(", " ", ")")
}
case class ConstructorInvocation3(constructor : String,
    a : CoqTerm, b : CoqTerm, c : CoqTerm) extends ConstructorInvocation {
  override def toString =
    Seq(constructor, a, b, c).mkString("(", " ", ")")
}
case class ConstructorInvocation4(constructor : String,
    a : CoqTerm, b : CoqTerm, c : CoqTerm, d : CoqTerm)
        extends ConstructorInvocation {
  override def toString =
    Seq(constructor, a, b, c, d).mkString("(", " ", ")")
}