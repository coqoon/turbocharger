package dk.itu.turbocharger.java

import org.eclipse.jdt.core.dom.{ASTNode, ASTVisitor}

object ASTUtilities {
  class ChildCollector[B](
      collector : ASTNode => Option[B]) extends ASTVisitor {
    private var children : List[B] = List()

    private var accept = true
    override def preVisit2(n : ASTNode) = {
      if (accept) {
        try {
          children = List()
          true
        } finally {
          accept = false
        }
      } else {
        collector.apply(n).foreach(children :+= _)
        false
      }
    }

    def result() = children
  }
  object ChildCollector {
    def apply[B](collector : PartialFunction[ASTNode, B]) =
      new ChildCollector(collector.lift)
  }

  def children[T](n : ASTNode)(implicit a0 : Manifest[T]) = {
    val q = ChildCollector {
      case a : T => a
    }
    n.accept(q)
    q.result
  }
}