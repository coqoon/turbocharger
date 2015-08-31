package dk.itu.turbocharger.coq

import dk.itu.coqoon.ui.pide.SessionManager
import isabelle._

/* A DispatchPool is the Turbocharger representation of a PIDE document. It
 * automatically assigns PIDE identifiers to commands, adding them to an
 * internal pool of commands, and converts sequences of commands into PIDE
 * document edits which are then dispatched to the prover. */

/* @name will be used as a PIDE document node name, so it should be the name of
 * a Coq source file. */
class DispatchPool(session : SessionManager, val name : String) {
  import DispatchPool._

  private var nodeName = name
  def getNodeName() = nodeName
  def setNodeName(name : String) = (nodeName = name)

  private var commandMappings : Map[String, Long] = Map()
  def getID(content : String) = commandMappings.get(content)
  def requireID(content : String) = commandMappings.get(content) match {
    case Some(id) =>
      id
    case None =>
      val id = define_command(content)
      commandMappings += (content -> id)
      id
  }

  private var lastDocumentID = 0L

  var lastCommands : Seq[Command] = Seq()

  def makeZanyDiff(a : Seq[Command], b : Seq[Command]) =
    PIDEDiff.makeZanyDiff(
        (a : Command, b : Command) => (a.id == b.id))(a, b).toList

  def defineDocument(document : Seq[String]) =
    session.executeWithSessionLock(session => {
      import XML.Encode._

      val commands = document.map(c => Command(
          requireID(c), Document.Node.Name(getNodeName),
          List(), parseSpan(c))).toList
      val diff = makeZanyDiff(lastCommands, commands)

      if (!diff.isEmpty) {
        val documentID = Document_ID.make()
        new ProtocolPunt("coq", session).update(
            lastDocumentID, documentID,
            List(
              (Document.Node.Name(getNodeName),
               Document.Node.Edits[(Option[Command], Option[Command]),
                 Command.Perspective](diff))))

        lastDocumentID = documentID
        lastCommands = commands
      }
    })

  private def define_command(content : String) =
    session.executeWithSessionLock(session => {
      val id = Document_ID.make()
      new ProtocolPunt("coq", session).define_command(Command(
          id, Document.Node.Name(getNodeName), List(), parseSpan(content)))
      id
    })
}
object DispatchPool {
  private val syntax = new isabelle.Coq_Syntax
  def parseSpan(content : String) = syntax.parse_spans(content).find(
      _.kind != isabelle.Command_Span.Ignored_Span).get
}

class ProtocolPunt(name : String, session : Session) extends Protocol {
  override def encode(s : String) = s
  override def decode(s : String) = s

  override def protocol_command(name : String, args : String*) =
    session.protocol_command(this.name, name, args : _*)
  /* This seems to be only used for blobs, which we don't care about */
  override def protocol_command_bytes(name : String, args : Bytes*) = ???

  override def define_command(command : Command) =
    protocol_command(
        "Document.define_command",
        Document_ID(command.id), 
        YXML.string_of_body(XML.Encode.bool(command.is_ignored)), 
        encode(command.source))
}