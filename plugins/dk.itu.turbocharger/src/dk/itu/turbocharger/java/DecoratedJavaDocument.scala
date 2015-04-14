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
  import dk.itu.turbocharger.coq._
  import dk.itu.turbocharger.coq.Charge._
  import org.eclipse.jdt.core.dom.TypeDeclaration

  def generateCompletePIDEDocument(doc : DecoratedJavaDocument) = {
    doc.getCompilationUnit.accept(new InfoVisitor)
    for (t <- children[TypeDeclaration](doc.getCompilationUnit))
      generateDefinitionsForType(t)
  }

  def generateDefinitionsForType(t : TypeDeclaration) : Unit = {
    if (!t.typeParameters.isEmpty)
      throw UnsupportedException(t,
          "Type parameters are not supported")
    t.getTypes.foreach(generateDefinitionsForType)
    import org.eclipse.jdt.core.dom.{Modifier, MethodDeclaration,
      SingleVariableDeclaration, VariableDeclarationFragment}
    import scala.collection.JavaConversions._
    var methods : Map[StringTerm, IdentifierTerm] = Map()
    for (method <- children[MethodDeclaration](t)) {
      try {
        if (!method.typeParameters.isEmpty)
          throw UnsupportedException(method,
              "Type parameters are not supported")
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

  import cmd_j._
  import val_j._
  import dexpr_j._

  def rvisitor(m : MethodDeclaration) : dexpr_j = {
    /* Constructors don't technically return anything */
    if (m.isConstructor)
      return E_val(nothing)
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
      cseq(b.statements.flatMap(TryCast[Statement]).map(svisitor(_)) : _*)
    case i : IfStatement =>
      cif(evisitor(i.getExpression),
          svisitor(i.getThenStatement), svisitor(i.getElseStatement()))
    case w : WhileStatement =>
      cwhile(evisitor(w.getExpression), svisitor(w.getBody))
    case d : DoStatement =>
      cseq(svisitor(d.getBody),
           cwhile(evisitor(d.getExpression), svisitor(d.getBody)))
    case f : ForStatement =>
      import scala.collection.JavaConversions._
      var result = List[cmd_j]()
      f.initializers.toList match {
        case (v : VariableDeclarationExpression) :: Nil =>
          result ++= handleLocalFragments(v.fragments.flatMap(
              TryCast[VariableDeclarationFragment]).toList)
        case f if f.forall(_.isInstanceOf[Assignment]) =>
          result ++= f.map(a => handleAssignment(a.asInstanceOf[Assignment]))
      }
      val updaters = f.updaters.flatMap(
          TryCast[Expression]).map(handleExpressionStatement).toList
      result :+=
        cwhile(evisitor(f.getExpression),
               cseq(svisitor(f.getBody), cseq(updaters : _*)))
      cseq(result : _*)
    case v : VariableDeclarationStatement =>
      import scala.collection.JavaConversions._
      cseq(handleLocalFragments(v.fragments.flatMap(
          TryCast[VariableDeclarationFragment]).toList) : _*)
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
      expandNameLike(n) match {
        case sn :: Nil =>
          E_var(sn)
        case qn :: rest =>
          throw UnsupportedException(n,
              "Field accesses cannot appear as part of an expression")
        case Nil =>
          throw UnsupportedException(n,
              "This name specifies neither a variable nor a field")
      }
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
        cseq(handleLocalFragments(v.fragments.flatMap(
            TryCast[VariableDeclarationFragment]).toList) : _*)
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
    val leftName = expandNameLike(a.getLeftHandSide)
    (leftName, a.getOperator, a.getRightHandSide) match {
      case (_, ASSIGN, _) =>
        handlePotentialFieldAssignment(a.getLeftHandSide, a.getRightHandSide)
      case (ls :: Nil,
            op @ (PLUS_ASSIGN | MINUS_ASSIGN | TIMES_ASSIGN),
            e : Expression) =>
        val exprv = op match {
          case PLUS_ASSIGN =>
            E_plus(E_var(ls), evisitor(e))
          case MINUS_ASSIGN =>
            E_minus(E_var(ls), evisitor(e))
          case TIMES_ASSIGN =>
            E_times(E_var(ls), evisitor(e))
          case _ =>
            throw UnsupportedException(a,
                "Compound assignment operators of this form are not supported")
        }
        cassign(ls, exprv)
      case q =>
        throw UnsupportedException(a,
            "Assignments of this form are not supported")
    }
  }

  def handleLocalFragments(
      fragments : List[VariableDeclarationFragment]) : List[cmd_j] =
    for (f <- fragments)
      yield handlePotentialFieldAssignment(f.getName, f.getInitializer)

  def handlePotentialFieldAssignment(
      left : ASTNode, right : Expression) : cmd_j = {
    val (lhs, rhs) = (expandNameLike(left), expandNameLike(right))
    (lhs, rhs) match {
      case ((lq :: ls :: rest), _)
          if rest != Nil =>
        throw UnsupportedException(left,
            "The left hand side of this assignment is too deeply qualified")
      case (_, (rq :: rs :: rest))
          if rest != Nil =>
        throw UnsupportedException(right,
            "The right hand side of this assignment is too deeply qualified")
      case ((ls :: lq), (rs :: rq))
          if lq != Nil && rq != Nil =>
        throw UnsupportedException(left.getParent,
            "Assigning from one field directly to another is not supported")
      case ((lq :: ls :: Nil), (rs :: Nil)) =>
        cwrite(lq, ls, E_var(rs))
      case ((ls :: Nil), (rs :: Nil)) =>
        cassign(ls, E_var(rs))
      case ((ls :: Nil), (rs :: rq :: Nil)) =>
        cread(ls, rs, rq)
      case ((ls :: Nil), (Nil)) =>
        handleLocalAssignment(ls, right)
      case _ =>
        println("???", left, lhs, right, rhs)
        throw UnsupportedException(left.getParent,
            "This potential field assignment is not supported")
    }
  }

  def handleLocalAssignment(n : String, e : Expression) : cmd_j = {
    Option(e) match {
      case Some(c : ClassInstanceCreation)
          if c.getType.isSimpleType =>
        calloc(n, c.getType.asInstanceOf[
          SimpleType].getName.asInstanceOf[SimpleName].getIdentifier)
      case Some(c : ClassInstanceCreation)
          if c.getType.isParameterizedType =>
        throw UnsupportedException(c,
            "Type parameters are not supported")
      case Some(m : MethodInvocation) =>
        if (!m.typeArguments.isEmpty)
          throw UnsupportedException(m,
              "Type parameters are not supported")
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
              n,
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
          cdcall(n, target, binding.getName, arguments)
        }
      case Some(q) =>
        cassign(n, evisitor(q))
      case None =>
        cassign(n, E_val(nothing))
    }
  }

  def variableBinding(n : Name) =
    TryCast[IVariableBinding](n.resolveBinding)

  def expandNameLike(n : ASTNode) : List[String] = n match {
    case sn : SimpleName
        if variableBinding(sn).exists(_.isField) =>
      List("this", sn.getIdentifier)
    case sn : SimpleName =>
      List(sn.getIdentifier)
    case qn : QualifiedName =>
      expandNameLike(qn.getQualifier) :+ qn.getName.getIdentifier
    case fa : FieldAccess =>
      expandNameLike(fa.getExpression) :+ fa.getName.getIdentifier
    case te : ThisExpression =>
      Option(te.getQualifier).toList.flatMap(expandNameLike) :+ "this"
    case _ =>
      List()
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