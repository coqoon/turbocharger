package dk.itu.turbocharger.java

object JavaDefinitions {
  import dk.itu.turbocharger.coq.Charge._
  import dk.itu.coqoon.core.utilities.TryCast
  import org.eclipse.jdt.core.dom._

  import cmd_j._
  import val_j._
  import dexpr_j._

  def rvisitor(m : MethodDeclaration) : dexpr_j = {
    /* Constructors don't technically return anything */
    if (m.isConstructor)
      return E_val(nothing)
    m.getReturnType2 match {
      case p : PrimitiveType
          if p.getPrimitiveTypeCode == PrimitiveType.VOID =>
        E_val(nothing)
      case t =>
        var rs : Option[ReturnStatement] = None
        m.accept(new ASTVisitor {
          override def preVisit(n : ASTNode) = rs.foreach(e =>
            throw UnsupportedException(e,
                "Early return from a method is not supported"))
          override def visit(r : ReturnStatement) = {
            val parent = Option(r.getParent)
            (parent, parent.map(_.getParent)) match {
              case (Some(b : Block), Some(m : MethodDeclaration)) =>
                rs = Some(r)
              case _ =>
                throw UnsupportedException(r,
                    "Conditional or otherwise nested returns are not " +
                    "supported")
            }
            false
          }
        })
        rs match {
          case Some(r) =>
            evisitor(r.getExpression)
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
    case n : NullLiteral =>
      E_val(null_j)
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
            val binding = variableBinding(n)
            val (_ :+ simplePart) = expandNameLike(n)
            if (binding.exists(!_.isField)) {
              cassign(simplePart, op match {
                case DECREMENT =>
                  E_minus(E_var(simplePart), E_val(vint(1)))
                case INCREMENT =>
                  E_plus(E_var(simplePart), E_val(vint(1)))
              })
            } else throw UnsupportedException(n,
                "This name does not refer to a local variable in scope")
          case (q, _) =>
            throw UnsupportedException(a,
                "Postfix expression statements of this form are not supported")
        }
      case a : PrefixExpression =>
        import PrefixExpression.Operator._
        (a.getOperator, a.getOperand) match {
          case (op @ (DECREMENT | INCREMENT), n : Name) =>
            val binding = variableBinding(n)
            val (_ :+ simplePart) = expandNameLike(n)
            if (binding.exists(!_.isField)) {
              cassign(simplePart, op match {
                case DECREMENT =>
                  E_minus(E_var(simplePart), E_val(vint(1)))
                case INCREMENT =>
                  E_plus(E_var(simplePart), E_val(vint(1)))
              })
            } else throw UnsupportedException(n,
                "This name does not refer to a local variable in scope")
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
      case ((lq :: ls :: Nil), (rs :: Nil)) => /* local variable to field */
        cwrite(lq, ls, E_var(rs))
      case ((ls :: Nil), (rs :: Nil)) => /* local variable to local variable */
        cassign(ls, E_var(rs))
      case ((ls :: Nil), (rs :: rq :: Nil)) => /* field to local variable */
        cread(ls, rs, rq)
      case ((ls :: Nil), (Nil)) => /* something else to local variable */
        handleLocalAssignment(ls, right)
      case ((lq :: ls :: rest), _) =>
        throw UnsupportedException(left,
            "The left hand side of this assignment is too deeply qualified")
      case (_, (rq :: rs :: rest)) =>
        throw UnsupportedException(right,
            "The right hand side of this assignment is too deeply qualified")
      case ((ls :: lq), (rs :: rq)) =>
        throw UnsupportedException(left.getParent,
            "Assigning from one field directly to another is not supported")
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

  def binding(n : Name) = Option(n.resolveBinding)
  def methodBinding(n : Name) = binding(n).flatMap(TryCast[IMethodBinding])
  def variableBinding(n : Name) = binding(n).flatMap(TryCast[IVariableBinding])

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