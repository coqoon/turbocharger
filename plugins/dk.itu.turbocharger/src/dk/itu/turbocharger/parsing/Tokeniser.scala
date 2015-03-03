package dk.itu.turbocharger.parsing

class Tokeniser {
  import Tokeniser._

  case class Token(label : String)
  case class InterestingTransition(transition : RType#Transition,
      begins : Token, leadin : Int) {
    addInterestingTransition(this)
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
              interestingTransitions.get(t).foreach(t => {
                val tokenContent = Substring(
                    input, lastTokenStart, position - t.leadin)
                if (tokenContent.length > 0)
                  nextToken = Some((lastTokenType, tokenContent.toString))
                lastTokenStart = Math.max(0, position - t.leadin)
                lastTokenType = t.begins
              })
              state = e
            case _ =>
              /* Oh no, bail out */
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

  private var interestingTransitions :
      Map[RType#Transition, InterestingTransition] = Map()
  private def addInterestingTransition(t : InterestingTransition) =
    interestingTransitions += (t.transition -> t)

  def tokens(initialToken : Token, start : RType#Execution,
      input : CharSequence) : Iterator[(Token, String)] =
    new TokenIterator(initialToken, start, input)
}
object Tokeniser {
  type RType = PushdownAutomaton[Char]
}