package dk.itu.turbocharger.java

import dk.itu.coqoon.core.utilities.CacheSlot
import dk.itu.turbocharger.parsing.{Tokeniser, DecoratedDocument}
import org.eclipse.core.resources.IFile

class DecoratedJavaDocument(
    file : IFile, tokens : Seq[(Tokeniser#Token, String)])
    extends DecoratedDocument(tokens) {
  def getCoqView() = new TypedView(Partitioning.Coq.ContentTypes.COQ)
  def getJavaView() = new TypedView(Partitioning.Java.ContentTypes.JAVA)

  private lazy val compilationUnit = {
    import org.eclipse.jdt.core.JavaCore
    import org.eclipse.jdt.core.dom.{AST, ASTParser, CompilationUnit}
    val parser = ASTParser.newParser(AST.JLS3)
    parser.setSource(getJavaView.get.toCharArray)
    parser.setProject(JavaCore.create(file.getProject))
    parser.setUnitName(file.getProjectRelativePath.removeFileExtension.
        addFileExtension(".java").makeAbsolute.toString)
    parser.setResolveBindings(true)
    parser.createAST(null).asInstanceOf[CompilationUnit]
  }
  def getCompilationUnit() = compilationUnit
}

import org.eclipse.jdt.core.dom.{ASTNode, ASTVisitor}

object DecoratedJavaCoqDocument {
  type class_j = String
  type var_j = String
  type ptr_j = (Int, class_j)
  type arrptr_j = Int
  type field_j = String
  type method_j = String

  trait CoqThing {
    override def toString : String
  }

  class Definition(val name : String, val value : CoqTerm) extends CoqThing {
    override def toString = s"""Definition ${name} := ${value}."""
  }

  trait CoqTerm extends CoqThing

  class IdentifierTerm(val a : String) extends CoqTerm {
    override def toString = s"""$a"""
  }

  class StringTerm(val a : String) extends CoqTerm {
    override def toString = s""""$a""""
  }
  implicit def stringToCt(a : String) = new StringTerm(a)

  class BooleanTerm(val a : Boolean) extends CoqTerm {
    override def toString = s"$a"
  }
  implicit def boolToCt(a : Boolean) = new BooleanTerm(a)

  class IntegerTerm(val a : Int) extends CoqTerm {
    override def toString = s"$a"
  }
  implicit def intToCt(a : Int) = new IntegerTerm(a)

  class ListTerm(val a : List[_ <: CoqTerm]) extends CoqTerm {
    override def toString = stringise(a)

    private def stringise(a : List[_ <: CoqTerm]) : String = a match {
      case a :: b =>
        s"(${a} :: ${stringise(b)})"
      case Nil =>
        "nil"
    }
  }
  implicit def listToCt(a : List[_ <: CoqTerm]) = new ListTerm(a)

  class TupleTerm(val a : Product) extends CoqTerm {
    override def toString = a.productIterator.mkString("(", ", ", ")")
  }
  implicit def tupleToCt(a : Product) = new TupleTerm(a)

  implicit def ptr_jToCt(a : ptr_j) = tupleToCt((a._1, a._2))

  sealed trait ConstructorInvocation extends CoqTerm
  class ConstructorInvocation0(constructor : String
      ) extends ConstructorInvocation {
    override def toString =
      Seq(constructor).mkString(" ")
  }
  class ConstructorInvocation1(constructor : String,
      a : CoqTerm) extends ConstructorInvocation {
    override def toString =
      Seq(constructor, a).mkString("(", " ", ")")
  }
  class ConstructorInvocation2(constructor : String,
      a : CoqTerm, b : CoqTerm) extends ConstructorInvocation {
    override def toString =
      Seq(constructor, a, b).mkString("(", " ", ")")
  }
  class ConstructorInvocation3(constructor : String,
      a : CoqTerm, b : CoqTerm, c : CoqTerm) extends ConstructorInvocation {
    override def toString =
      Seq(constructor, a, b, c).mkString("(", " ", ")")
  }
  class ConstructorInvocation4(constructor : String,
      a : CoqTerm, b : CoqTerm, c : CoqTerm, d : CoqTerm) extends ConstructorInvocation {
    override def toString =
      Seq(constructor, a, b, c, d).mkString("(", " ", ")")
  }

  sealed trait val_j extends CoqTerm with ConstructorInvocation
  case class vint(a : Int) extends ConstructorInvocation1(
      "vint", a) with val_j
  case class vbool(a : Boolean) extends ConstructorInvocation1(
      "vbool", a) with val_j
  case class vptr(a : ptr_j) extends ConstructorInvocation1(
      "vptr", a) with val_j
  case class varr(a : arrptr_j) extends ConstructorInvocation1(
      "varr", a) with val_j
  case object nothing extends ConstructorInvocation0(
      "nothing") with val_j

  sealed trait dexpr_j extends CoqTerm with ConstructorInvocation
  case class E_val(a : val_j) extends ConstructorInvocation1(
      "E_val", a) with dexpr_j
  case class E_var(a : var_j) extends ConstructorInvocation1(
      "E_var", a) with dexpr_j
  case class E_plus(a : dexpr_j, b : dexpr_j) extends ConstructorInvocation2(
      "E_plus", a, b) with dexpr_j
  case class E_minus(a : dexpr_j, b : dexpr_j) extends ConstructorInvocation2(
      "E_minus", a, b) with dexpr_j
  case class E_times(a : dexpr_j, b : dexpr_j) extends ConstructorInvocation2(
      "E_times", a, b) with dexpr_j
  case class E_and(a : dexpr_j, b : dexpr_j) extends ConstructorInvocation2(
      "E_and", a, b) with dexpr_j
  case class E_or(a : dexpr_j, b : dexpr_j) extends ConstructorInvocation2(
      "E_or", a, b) with dexpr_j
  case class E_not(a : dexpr_j) extends ConstructorInvocation1(
      "E_not", a) with dexpr_j
  case class E_lt(a : dexpr_j, b : dexpr_j) extends ConstructorInvocation2(
      "E_lt", a, b) with dexpr_j
  case class E_eq(a : dexpr_j, b : dexpr_j) extends ConstructorInvocation2(
      "E_eq", a, b) with dexpr_j

  sealed trait cmd_j extends CoqTerm with ConstructorInvocation
  case class cassign(a : var_j, b : dexpr_j) extends ConstructorInvocation2(
      "cassign", a, b) with cmd_j
  case object cskip extends ConstructorInvocation0(
      "cskip") with cmd_j
  case class cseq(a : cmd_j, b : cmd_j) extends ConstructorInvocation2(
      "cseq", a, b) with cmd_j
  case class cif(a : dexpr_j, b : cmd_j, c : cmd_j) extends ConstructorInvocation3(
      "cif", a, b, c) with cmd_j
  case class cwhile(a : dexpr_j, b : cmd_j) extends ConstructorInvocation2(
      "cwhile", a, b) with cmd_j
  case class cread(a : var_j, b : var_j, c : field_j) extends ConstructorInvocation3(
      "cread", a, b, c) with cmd_j
  case class cwrite(a : var_j, b : field_j, c : dexpr_j) extends ConstructorInvocation3(
      "cwrite", a, b, c) with cmd_j
  case class carrread(a : var_j, b : var_j, c : List[dexpr_j]) extends ConstructorInvocation3(
      "carrread", a, b, c) with cmd_j
  case class carrwrite(a : var_j, b : List[dexpr_j], c : dexpr_j) extends ConstructorInvocation3(
      "carrwrite", a, b, c) with cmd_j
  case class carralloc(a : var_j, b : dexpr_j) extends ConstructorInvocation2(
      "carralloc", a, b) with cmd_j
  case class calloc(a : var_j, b : class_j) extends ConstructorInvocation2(
      "calloc", a, b) with cmd_j
  case class cdcall(a : var_j, b : var_j, c : method_j, d : List[dexpr_j]) extends ConstructorInvocation4(
      "cdcall", a, b, c, d) with cmd_j
  case class cscall(a : var_j, b : class_j, c : method_j, d : List[dexpr_j]) extends ConstructorInvocation4(
      "cscall", a, b, c, d) with cmd_j
  case class cassert(a : dexpr_j) extends ConstructorInvocation1(
      "cassert", a) with cmd_j

  def cseqise(cs : List[cmd_j]) : cmd_j = cs match {
    case Nil =>
      cskip
    case `cskip` :: a =>
      cseqise(a)
    case a :: `cskip` :: Nil =>
      a
    case a :: Nil =>
      a
    case a :: b =>
      cseq(a, cseqise(b))
  }

  import dk.itu.coqoon.core.utilities.TryCast
  import org.eclipse.jdt.core.dom._

  def getParts(n : Name) = {
    val t = n match {
      case s : SimpleName =>
        ("this", s.getIdentifier)
      case q : QualifiedName =>
        (q.getQualifier.getFullyQualifiedName.toString,
            q.getName.getIdentifier)
    }
    (t._1, t._2, n.resolveBinding.asInstanceOf[IVariableBinding])
  }

  def rvisitor(m : MethodDeclaration) : dexpr_j = {
    import scala.collection.JavaConversions._
    m.getReturnType2 match {
      case p : PrimitiveType
          if p.getPrimitiveTypeCode == PrimitiveType.VOID =>
        E_val(nothing)
      case t =>
        m.getBody.statements.last match {
          case r : ReturnStatement =>
            evisitor(r.getExpression)
          case _ =>
            ???
        }
    }
  }

  def svisitor(a : Statement) : cmd_j = a match {
    case b : Block =>
      import scala.collection.JavaConversions._
      cseqise(b.statements.flatMap(TryCast[Statement]).map(svisitor(_)).toList)
    case i : IfStatement =>
      cif(evisitor(i.getExpression),
          svisitor(i.getThenStatement), svisitor(i.getElseStatement()))
    case w : WhileStatement =>
      cwhile(evisitor(w.getExpression), svisitor(w.getBody))
    case d : DoStatement =>
      cseqise(List(
          svisitor(d.getBody),
          cwhile(evisitor(d.getExpression), svisitor(d.getBody))))
    case f : ForStatement =>
      import scala.collection.JavaConversions._
      var result = List[cmd_j]()
      f.initializers.toList match {
        case (v : VariableDeclarationExpression) :: Nil =>
          result ++= handleFragments(v.fragments.flatMap(
              TryCast[VariableDeclarationFragment]).toList)
        case f if f.forall(_.isInstanceOf[Assignment]) =>
          result ++= f.map(a => handleAssignment(a.asInstanceOf[Assignment]))
      }
      val updaters = f.updaters.flatMap(
          TryCast[Expression]).map(handleExpressionStatement).toList
      result :+=
        cwhile(evisitor(f.getExpression),
               cseqise(List(
                   svisitor(f.getBody),
                   cseqise(updaters))))
      cseqise(result)
    case v : VariableDeclarationStatement =>
      import scala.collection.JavaConversions._
      cseqise(handleFragments(v.fragments.flatMap(
          TryCast[VariableDeclarationFragment]).toList))
    case e : ExpressionStatement =>
      handleExpressionStatement(e.getExpression)
    case e : EmptyStatement =>
      cskip
    case r : ReturnStatement =>
      /* Ideally we would handle this here, but cmd_j doesn't have any way of
       * representing return statements */
      cskip
    case q =>
      println(s"??? svisitor ${q}")
      ???
  }

  def evisitor(a : Expression) : dexpr_j = a match {
    case n : NumberLiteral =>
      E_val(vint(Integer.parseInt(n.getToken)))
    case b : BooleanLiteral =>
      E_val(vbool(b.booleanValue))
    case n : SimpleName =>
      E_var(n.getIdentifier)
    case p : PrefixExpression =>
      import PrefixExpression.Operator._
      (p.getOperator, p.getOperand) match {
        case (NOT, e) =>
          E_not(evisitor(e))
        case _ =>
          ???
      }
    case p : ParenthesizedExpression =>
      evisitor(p.getExpression)
    case i : InfixExpression =>
      import InfixExpression.Operator._
      (i.getLeftOperand, i.getOperator, i.getRightOperand) match {
        case (l, PLUS, r) =>
          E_plus(evisitor(l), evisitor(r))
        case (l, MINUS, r) =>
          E_minus(evisitor(l), evisitor(r))
        case (l, TIMES, r) =>
          E_times(evisitor(l), evisitor(r))
        case (l, LESS, r) =>
          E_lt(evisitor(l), evisitor(r))
        case (l, EQUALS, r) =>
          E_eq(evisitor(l), evisitor(r))
        case (l, AND, r) =>
          E_and(evisitor(l), evisitor(r))
        case (l, OR, r) =>
          E_or(evisitor(l), evisitor(r))

        /* Derived: */
        case (l, LESS_EQUALS, r) =>
          E_or(E_eq(evisitor(l), evisitor(r)),
               E_lt(evisitor(l), evisitor(r)))
        case (l, GREATER, r) =>
          E_not(E_or(E_eq(evisitor(r), evisitor(l)),
                     E_lt(evisitor(r), evisitor(l))))
        case (l, GREATER_EQUALS, r) =>
          E_not(E_lt(evisitor(r), evisitor(l)))
        case (l, NOT_EQUALS, r) =>
          E_not(E_eq(evisitor(l), evisitor(r)))

        case q =>
          ???
      }
    case q =>
      println(s"??? evisitor ${q}")
      ???
  }

  def handleExpressionStatement(e : Expression) : cmd_j =
    e match {
      case v : VariableDeclarationExpression =>
        import scala.collection.JavaConversions._
        cseqise(handleFragments(v.fragments.flatMap(
            TryCast[VariableDeclarationFragment]).toList))
      case a : Assignment =>
        handleAssignment(a)

      /* These translations are only safe because these expressions are
       * standalone statements, so we can discard their results */
      case a : PostfixExpression =>
        import PostfixExpression.Operator._
        (a.getOperand, a.getOperator) match {
          case (n : Name, op @ (DECREMENT | INCREMENT)) =>
            val (qualPart, simplePart, binding) = getParts(n)
            val exprv = op match {
              case DECREMENT if !binding.isField =>
                E_minus(E_var(simplePart), E_val(vint(1)))
              case INCREMENT if !binding.isField =>
                E_plus(E_var(simplePart), E_val(vint(1)))
            }
            if (!binding.isField) {
              cassign(simplePart, exprv)
            } else ???
          case q =>
            ???
        }
      case a : PrefixExpression =>
        import PrefixExpression.Operator._
        (a.getOperator, a.getOperand) match {
          case (op @ (DECREMENT | INCREMENT), n : Name) =>
            val (qualPart, simplePart, binding) = getParts(n)
            val exprv = op match {
              case DECREMENT if !binding.isField =>
                E_minus(E_var(simplePart), E_val(vint(1)))
              case INCREMENT if !binding.isField =>
                E_plus(E_var(simplePart), E_val(vint(1)))
            }
            if (!binding.isField) {
              cassign(simplePart, exprv)
            } else ???
          case q =>
            ???
        }

      case q =>
        ???
    }

  def handleAssignment(a : Assignment) : cmd_j = {
    import Assignment.Operator._
    (a.getLeftHandSide, a.getOperator, a.getRightHandSide) match {
      case (n : SimpleName, ASSIGN, c : ClassInstanceCreation)
          if c.getType.isSimpleType =>
        calloc(n.getIdentifier, c.getType.asInstanceOf[
          SimpleType].getName.asInstanceOf[SimpleName].getIdentifier)
      case (n : Name,
            op @ (ASSIGN | PLUS_ASSIGN | MINUS_ASSIGN | TIMES_ASSIGN),
            e : Expression) =>
        val (qualPart, simplePart, binding) = getParts(n)
        val exprv = op match {
          case ASSIGN =>
            evisitor(e)
          case PLUS_ASSIGN if !binding.isField =>
            E_plus(E_var(simplePart), evisitor(e))
          case MINUS_ASSIGN if !binding.isField =>
            E_minus(E_var(simplePart), evisitor(e))
          case TIMES_ASSIGN if !binding.isField =>
            E_times(E_var(simplePart), evisitor(e))
          case _ => ???
        }
        if (binding.isField) {
          cwrite(qualPart, simplePart, exprv)
        } else cassign(simplePart, exprv)
      case q =>
        ???
    }
  }

  def handleFragments(
      fragments : List[VariableDeclarationFragment]) : List[cmd_j] =
    for (f <- fragments)
      yield (f.getInitializer match {
        case c : ClassInstanceCreation
            if c.getType.isSimpleType =>
          calloc(f.getName.getIdentifier, c.getType.asInstanceOf[
            SimpleType].getName.asInstanceOf[SimpleName].getIdentifier)
        case q =>
          cassign(f.getName.getIdentifier, evisitor(q))
      })
}

class InfoVisitor extends ASTVisitor {
  private var stack : List[ASTNode] = List()

  private var accept = true
  override def preVisit(n : ASTNode) = {
    stack +:= n
    println(stack.map(_.getClass.getName))
  }

  override def postVisit(n : ASTNode) = {
    stack = stack.tail
  }
}