package dk.itu.turbocharger.java

import dk.itu.coqoon.core.utilities.CacheSlot
import dk.itu.turbocharger.parsing.{Tokeniser, DecoratedDocument}
import org.eclipse.core.resources.IFile

class DecoratedJavaDocument(
    val file : IFile, tokens : Seq[(Tokeniser#Token, String)])
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
  import DecoratedDocument.Region
  import dk.itu.coqoon.core.model.ICoqModel
  import dk.itu.coqoon.core.utilities.TryCast
  import dk.itu.turbocharger.coq._
  import dk.itu.turbocharger.coq.Charge._
  import org.eclipse.jdt.core.dom.TypeDeclaration

  /* Returns a sequence of commands and, if appropriate, the document regions
   * they're derived from. */
  def generateCompletePIDEDocument(doc : DecoratedJavaDocument) :
      Seq[(CoqCommand, Map[Region, Int])] = {
    val decls = children[TypeDeclaration](doc.getCompilationUnit)

    val loadPath =
      Option(ICoqModel.toCoqProject(doc.file.getProject)).toSeq.flatMap(
          p => p.getLoadPath.flatMap(lpe =>
            lpe.asCommands.map(
                c => (ArbitrarySentence(c), Map.empty[Region, Int]))))
    val init = decls.headOption.toSeq.flatMap(firstClass => {
      val coqView = doc.getCoqView
      val javaView = doc.getJavaView
      val initEnd = javaView.toDocumentOffset(firstClass.getStartPosition).get

      /* XXX: This is literally copied-and-pasted from
       * ProofExtraction.extractProof. Surely we can do better? */
      val pt =
        doc.getPartialTokens(Region(0, length = initEnd)) match {
          case Some((start, tokens)) =>
            DecoratedDocument.withPositions(
                start, tokens).filter(t => coqView.contains(t._2))
          case _ =>
            Seq()
        }
      import dk.itu.coqoon.core.utilities.Substring
      import isabelle.Command_Span
      pt flatMap {
        case (start, (token, content)) =>
          /* Strip the leading and trailing antiquote bits from this token
           * and extract all the sentences that it contains */
          var pos = 2
          import dk.itu.turbocharger.coq.CommonSyntax.parse_spans
          val spans = parse_spans(Substring(content, 2, content.length - 2))
          (for (Command_Span.Span(kind, body) <- spans)
            yield {
              val c = body.map(_.content).mkString
              try {
                if (kind != Command_Span.Ignored_Span) {
                  Some(ArbitrarySentence(c.toString),
                      Map(Region(0, length = c.length) -> (start + pos)))
                } else None
              } finally pos += c.length
            }).flatten
      }
    })

    loadPath ++ init ++ decls.flatMap(t =>
      generateDefinitionsForType(t).map((_, Map.empty[Region, Int])) ++
        extractMethodProofs(doc, t))
  }

  def extractMethodProofs(doc : DecoratedJavaDocument,
      t : TypeDeclaration) : Seq[(CoqCommand, Map[Region, Int])] = {
    val coqView = doc.getCoqView
    val javaView = doc.getJavaView

    var lastEnd = t.getStartPosition
    import org.eclipse.jdt.core.dom.MethodDeclaration
    val proofs =
      for (method <- children[MethodDeclaration](t)) yield {
        import org.eclipse.jdt.core.dom.IMethodBinding
        println(method.resolveBinding)

        import dk.itu.turbocharger.coq.ProofExtraction.extractProof
        try {
          /* Specifications lie in the region between the end of the last
           * method and the start of this one... */
          val sr = javaView.toSingleDocumentRegion(
              Region(lastEnd, length = method.getStartPosition - lastEnd))
          /* ... while proof content must fall somewhere in the body of this
           * method. */
          val pr = javaView.toSingleDocumentRegion(
              Region(method.getStartPosition, length = method.getLength))
          extractProof(method, sr, pr, doc, doc.getJavaView, doc.getCoqView)
        } finally {
          lastEnd = method.getStartPosition + method.getLength
        }
      }
    proofs.flatten
  }

  def generateDefinitionsForType(t : TypeDeclaration) : Seq[Definition] = {
    import JavaDefinitions._

    if (!t.typeParameters.isEmpty)
      throw UnsupportedException(t,
          "Type parameters are not supported")
    val subtypeDefinitions = t.getTypes.flatMap(generateDefinitionsForType)
    import org.eclipse.jdt.core.dom.{Modifier, MethodDeclaration,
      SingleVariableDeclaration, VariableDeclarationFragment}
    import scala.collection.JavaConversions._
    var methods : Seq[(StringTerm, Definition, Definition)] = Seq()
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

        methods :+= (StringTerm(method.getName.getIdentifier), mb, md)
      } catch {
        case n : NotImplementedError =>
          Console.err.println("It is NO GOOD")
      }
    }

    val methodDefinitions = methods.flatMap(m => Seq(m._2, m._3))
    val fieldFragments = t.getFields.flatMap(
        _.fragments.flatMap(TryCast[VariableDeclarationFragment])).toList
    subtypeDefinitions ++ methodDefinitions :+ new Definition(
        t.getName.getIdentifier + "_Class",
        new ConstructorInvocation2(
            "Build_Class",
            ListTerm(fieldFragments.map(
                f => StringTerm(f.getName.getIdentifier))),
            ListTerm(
                methods.map(m =>
                  TupleTerm(m._1, IdentifierTerm(m._3.name))).toList)))
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