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
  import ASTUtilities.children
  import dk.itu.coqoon.core.utilities.TryCast
  import org.eclipse.jdt.core.dom.TypeDeclaration

  def generateCompletePIDEDocument(doc : DecoratedJavaDocument) = {
    doc.getCompilationUnit.accept(new InfoVisitor)
    for (t <- children[TypeDeclaration](doc.getCompilationUnit))
      generateDefinitionsForType(t)
  }

  def generateDefinitionsForType(t : TypeDeclaration) : Unit = {
    t.getTypes.foreach(generateDefinitionsForType)
    import org.eclipse.jdt.core.dom.{Modifier, MethodDeclaration,
      SingleVariableDeclaration, VariableDeclarationFragment}
    import scala.collection.JavaConversions._
    var methods : Map[StringTerm, IdentifierTerm] = Map()
    for (method <- children[MethodDeclaration](t)) {
      try {
        val parameters = {
          val p = method.parameters.flatMap(
              TryCast[SingleVariableDeclaration]).map(
                  p => StringTerm(p.getName.getIdentifier)).toList
          if (method.modifiers.flatMap(TryCast[Modifier]).exists(_.isStatic)) {
            p
          } else StringTerm("this") +: p
        }
        val b = method.resolveBinding
        /* We might want to use getQualifiedName in future here */
        val definitionId =
          s"${b.getDeclaringClass.getName}_${b.getName}"

        val mb =
          new Definition(definitionId + "_body", svisitor(method.getBody))
        val md = new Definition(
            definitionId + "_Method",
            new ConstructorInvocation3(
                "Build_Method",
                ListTerm(parameters),
                IdentifierTerm(definitionId + "_body"),
                rvisitor(method)))
        println(mb)
        println(md)

        methods +=
          StringTerm(method.getName.getIdentifier) -> IdentifierTerm(md.name)
      } catch {
        case n : NotImplementedError =>
          Console.err.println("It is NO GOOD")
      }
    }

    val fieldFragments = t.getFields.flatMap(
        _.fragments.flatMap(TryCast[VariableDeclarationFragment])).toList
    val cd = new Definition(
        t.getName.getIdentifier + "_Class",
        new ConstructorInvocation2(
            "Build_Class",
            ListTerm(fieldFragments.map(
                f => StringTerm(f.getName.getIdentifier))),
            ListTerm(
                methods.map(m => TupleTerm(m._1, m._2)).toList)))
    println(cd)
  }

  type class_j = String
  type var_j = String
  type ptr_j = (Int, class_j)
  type arrptr_j = Int
  type field_j = String
  type method_j = String

  trait CoqThing {
    override def toString : String
  }

  case class Definition(name : String, value : CoqTerm) extends CoqThing {
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

  sealed trait ConstructorInvocation extends CoqTerm
  case class ConstructorInvocation0(constructor : String
      ) extends ConstructorInvocation {
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
      a : CoqTerm, b : CoqTerm, c : CoqTerm, d : CoqTerm) extends ConstructorInvocation {
    override def toString =
      Seq(constructor, a, b, c, d).mkString("(", " ", ")")
  }

  sealed trait val_j extends CoqTerm
  object val_j {
    def vint(a : Int) : val_j =
      new ConstructorInvocation1("vint", ZTerm(a)) with val_j
    def vbool(a : Boolean) : val_j =
      new ConstructorInvocation1("vbool", BooleanTerm(a)) with val_j
    def vptr(a : ptr_j) : val_j =
      new ConstructorInvocation1("vptr", TupleTerm(a)) with val_j
    def varr(a : arrptr_j) : val_j =
      new ConstructorInvocation1("varr", NatTerm(a)) with val_j
    def nothing() : val_j = new ConstructorInvocation0("nothing") with val_j
  }

  sealed trait dexpr_j extends CoqTerm
  object dexpr_j {
    def E_val(a : val_j) : dexpr_j =
      new ConstructorInvocation1("E_val", a) with dexpr_j
    def E_var(a : var_j) : dexpr_j =
      new ConstructorInvocation1("E_var", StringTerm(a)) with dexpr_j
    def E_plus(a : dexpr_j, b : dexpr_j) : dexpr_j =
      new ConstructorInvocation2("E_plus", a, b) with dexpr_j
    def E_minus(a : dexpr_j, b : dexpr_j) : dexpr_j =
      new ConstructorInvocation2("E_minus", a, b) with dexpr_j
    def E_times(a : dexpr_j, b : dexpr_j) : dexpr_j =
      new ConstructorInvocation2("E_times", a, b) with dexpr_j
    def E_and(a : dexpr_j, b : dexpr_j) : dexpr_j =
      new ConstructorInvocation2("E_and", a, b) with dexpr_j
    def E_or(a : dexpr_j, b : dexpr_j) : dexpr_j =
      new ConstructorInvocation2("E_or", a, b) with dexpr_j
    def E_not(a : dexpr_j) : dexpr_j =
      new ConstructorInvocation1("E_not", a) with dexpr_j
    def E_lt(a : dexpr_j, b : dexpr_j) : dexpr_j =
      new ConstructorInvocation2("E_lt", a, b) with dexpr_j
    def E_eq(a : dexpr_j, b : dexpr_j) : dexpr_j =
      new ConstructorInvocation2("E_eq", a, b) with dexpr_j
  }

  sealed trait cmd_j extends CoqTerm with ConstructorInvocation
  object cmd_j {
    def cassign(a : var_j, b : dexpr_j) : cmd_j =
        new ConstructorInvocation2("cassign", StringTerm(a), b) with cmd_j
    def cskip() : cmd_j = new ConstructorInvocation0("cskip") with cmd_j
    def cseq(a : cmd_j, b : cmd_j) : cmd_j =
      new ConstructorInvocation2("cseq", a, b) with cmd_j
    def cif(a : dexpr_j, b : cmd_j, c : cmd_j) : cmd_j =
      new ConstructorInvocation3("cif", a, b, c) with cmd_j
    def cwhile(a : dexpr_j, b : cmd_j) : cmd_j =
      new ConstructorInvocation2("cwhile", a, b) with cmd_j
    def cread(a : var_j, b : var_j, c : field_j) : cmd_j =
      new ConstructorInvocation3("cread",
          StringTerm(a), StringTerm(b), StringTerm(c)) with cmd_j
    def cwrite(a : var_j, b : field_j, c : dexpr_j) : cmd_j =
      new ConstructorInvocation3("cwrite",
          StringTerm(a), StringTerm(b), c) with cmd_j
    def carrread(a : var_j, b : var_j, c : List[dexpr_j]) : cmd_j =
      new ConstructorInvocation3("carrread",
          StringTerm(a), StringTerm(b), ListTerm(c)) with cmd_j
    def carrwrite(a : var_j, b : List[dexpr_j], c : dexpr_j) : cmd_j =
      new ConstructorInvocation3("carrwrite",
          StringTerm(a), ListTerm(b), c) with cmd_j
    def carralloc(a : var_j, b : dexpr_j) : cmd_j =
      new ConstructorInvocation2("carralloc", StringTerm(a), b) with cmd_j
    def calloc(a : var_j, b : class_j) : cmd_j =
      new ConstructorInvocation2("calloc",
          StringTerm(a), StringTerm(b)) with cmd_j
    def cdcall(a : var_j, b : var_j, c : method_j, d : List[dexpr_j]) : cmd_j =
      new ConstructorInvocation4("cdcall",
          StringTerm(a), StringTerm(b), StringTerm(c), ListTerm(d)) with cmd_j
    def cscall(a : var_j,
        b : class_j, c : method_j, d : List[dexpr_j]) : cmd_j =
      new ConstructorInvocation4("cscall",
          StringTerm(a), StringTerm(b), StringTerm(c), ListTerm(d)) with cmd_j
    def cassert(a : dexpr_j) : cmd_j =
      new ConstructorInvocation1("cassert", a) with cmd_j
  }

  import cmd_j._
  import val_j._
  import dexpr_j._

  def cseqise(cs : List[cmd_j]) : cmd_j = cs match {
    case Nil =>
      cskip
    case (c : ConstructorInvocation0) :: a
        if c.constructor == "cskip" =>
      cseqise(a)
    case a :: (c : ConstructorInvocation0) :: Nil
        if c.constructor == "cskip" =>
      a
    case a :: Nil =>
      a
    case a :: b =>
      cseq(a, cseqise(b))
  }

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
        var expr : Option[(ReturnStatement, dexpr_j)] = None
        m.accept(new ASTVisitor {
          override def preVisit(n : ASTNode) = expr.foreach(e =>
            throw UnsupportedException(e._1,
                "Early return from a method is not supported"))
          override def visit(r : ReturnStatement) = {
            val parent = Option(r.getParent)
            (parent, parent.map(_.getParent)) match {
              case (Some(b : Block), Some(m : MethodDeclaration)) =>
                expr = Some((r, evisitor(r.getExpression)))
              case _ =>
                throw UnsupportedException(r,
                    "Conditional or otherwise nested returns are not " +
                    "supported")
            }
            false
          }
        })
        expr match {
          case Some((_, e)) =>
            e
          case None =>
            throw UnsupportedException(m,
                "Non-void methods must end with a return statement")
        }
    }
  }

  def svisitor(a : Statement) : cmd_j = a match {
    case null =>
      cskip
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
    case a : AssertStatement =>
      cassert(evisitor(a.getExpression))
    case q =>
      throw UnsupportedException(q,
          "Statements of this form are not supported")
  }

  def evisitor(a : Expression) : dexpr_j = a match {
    case t : ThisExpression
        if t.getQualifier() == null =>
      E_var("this")
    case n : NumberLiteral =>
      E_val(vint(Integer.parseInt(n.getToken)))
    case b : BooleanLiteral =>
      E_val(vbool(b.booleanValue))
    case n : SimpleName =>
      E_var(n.getIdentifier)
    case p : PrefixExpression =>
      import PrefixExpression.Operator._
      (p.getOperator, p.getOperand) match {
        case (PLUS, e) =>
          evisitor(e)
        case (MINUS, e) =>
          E_minus(E_val(vint(0)), evisitor(e))
        case (NOT, e) =>
          E_not(evisitor(e))
        case _ =>
          throw UnsupportedException(p,
              "Prefix expressions of this form are not supported")
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
          throw UnsupportedException(i,
              "Infix expressions of this form are not supported")
      }
    case q =>
      throw UnsupportedException(q,
          "Expressions of this form are not supported")
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
            } else throw UnsupportedException(n,
                "Only local variables can appear in expressions")
          case (q, _) =>
            throw UnsupportedException(a,
                "Postfix expression statements of this form are not supported")
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
            } else throw UnsupportedException(n,
                "Only local variables can appear in expressions")
          case (_, q) =>
            throw UnsupportedException(a,
                "Prefix expression statements of this form are not supported")
        }

      case q =>
        throw UnsupportedException(q,
            "Expression statements of this form are not supported")
    }

  def handleAssignment(a : Assignment) : cmd_j = {
    import Assignment.Operator._
    (a.getLeftHandSide, a.getOperator, a.getRightHandSide) match {
      case (n : SimpleName, ASSIGN, c : ClassInstanceCreation) =>
        handleUnpackedAssignment(n, c)
      case (n : SimpleName, ASSIGN, m : MethodInvocation) =>
        handleUnpackedAssignment(n, m)
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
          case _ =>
            throw UnsupportedException(n,
                "Assignment operators of this form are not supported")
        }
        if (binding.isField) {
          cwrite(qualPart, simplePart, exprv)
        } else cassign(simplePart, exprv)
      case q =>
        throw UnsupportedException(a,
            "Assignments of this form are not supported")
    }
  }

  def handleFragments(
      fragments : List[VariableDeclarationFragment]) : List[cmd_j] =
    for (f <- fragments)
      yield handleUnpackedAssignment(f.getName, f.getInitializer)

  def handleUnpackedAssignment(n : SimpleName, e : Expression) : cmd_j =
    Option(e) match {
      case Some(c : ClassInstanceCreation)
          if c.getType.isSimpleType =>
        calloc(n.getIdentifier, c.getType.asInstanceOf[
          SimpleType].getName.asInstanceOf[SimpleName].getIdentifier)
      case Some(m : MethodInvocation) =>
        import Modifier.isStatic
        import scala.collection.JavaConversions._
        val binding = m.resolveMethodBinding
        /* XXX: er, we probably need better error recovery than this! */
        if (binding == null)
          ???
        val arguments =
          m.arguments.flatMap(TryCast[Expression]).toList.map(evisitor)
        if (Modifier.isStatic(binding.getModifiers)) {
          cscall(
              n.getIdentifier,
              binding.getDeclaringClass.getName /* XXX: qualification */,
              binding.getName,
              arguments)
        } else {
          val target : var_j =
            Option(m.getExpression) match {
              case None =>
                "this"
              case Some(n : SimpleName) =>
                n.getIdentifier
              case _ =>
                throw UnsupportedException(m.getExpression,
                    "Using more than one level of indirection in method " +
                    "calls is not supported")
            }
          cdcall(n.getIdentifier, target, binding.getName, arguments)
        }
      case Some(q) =>
        cassign(n.getIdentifier, evisitor(q))
      case None =>
        cassign(n.getIdentifier, E_val(nothing))
    }

  class UnsupportedException(
      val node : ASTNode, val message : String) extends Exception(message)
  object UnsupportedException {
    def apply(node : ASTNode, message : String) =
      new UnsupportedException(node, message)
    def unapply(t : Throwable) = t match {
      case n : UnsupportedException =>
        Some((n.node, n.message))
      case _ => None
    }
  }
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