package dk.itu.turbocharger.parsing

class Tokeniser {
  import Tokeniser._
  import PushdownAutomaton.{State => Token}

  type TransitionInspector = Transition => Option[(Token, Int)]
  protected object TransitionInspector {
    def apply(transition : TransitionInspector) =
      addTransitionInspector(transition)
  }
  protected object InterestingTransition {
    def apply(transition : Transition, begins : Token, leadin : Int) =
      addTransitionInspector {
        case t if t == transition => Some((begins, leadin))
        case _ => None
      }
  }

  class TokenIterator(initialToken : Token, start : RType#Execution,
      input : CharSequence) extends Iterator[(Token, String)] {
    private var lastTokenStart = 0
    private var lastTokenType = initialToken
    private var position = 0
    private var state = start

    override def hasNext = {
      prime()
      nextToken != None
    }
    override def next : (Token, String) = {
      prime()
      nextToken match {
        case Some(t) =>
          nextToken = None
          return t
        case None =>
          nextToken.get
      }
    }

    private var nextToken : Option[(Token, String)] = None
    private def prime() : Unit = {
      import dk.itu.coqoon.core.utilities.Substring
      if (nextToken == None) {
        while (nextToken == None && position < input.length) {
          val c = input.charAt(position)
          state.accept(c) match {
            case Some((t, e)) =>
              position += 1
              transitionInspectors.flatMap(ti => ti(t)).headOption foreach {
                case (begins, leadin)
                    if lastTokenType != begins =>
                  val tokenContent = Substring(
                      input, lastTokenStart, position - leadin)
                  nextToken = Some((lastTokenType, tokenContent.toString))
                  lastTokenStart = Math.max(0, position - leadin)
                  lastTokenType = begins
                case _ =>
              }
              state = e
            case _ =>
              /* Oh no, bail out */
              ???
          }
        }

        if (nextToken == None) {
          /* Produce a final token if possible */
          val tokenContent =
            Substring(input, lastTokenStart, position)
          if (tokenContent.length > 0) {
            nextToken = Some((lastTokenType, tokenContent.toString))
            lastTokenStart = position
          }
        }
      }
    }
  }

  private var transitionInspectors : Seq[TransitionInspector] = Seq()
  private def addTransitionInspector(t : TransitionInspector) =
    transitionInspectors :+= t

  def tokens(start : RType#Execution,
      input : CharSequence) : Iterator[(Token, String)] =
    new TokenIterator(start.position, start, input)
}
object Tokeniser {
  protected type RType = PushdownAutomaton[Char]
  protected type Transition = PushdownAutomaton.Transition[Char]
}