package dk.itu.turbocharger.coq

import dk.itu.coqoon.ui.pide.SessionManager
import isabelle._

class DispatchPool {
  import DispatchPool._

  def getID(content : String) = commandMappings.get(content)
  def requireID(content : String) = commandMappings.get(content) match {
    case Some(id) =>
      id
    case None =>
      val id = define_command(content)
      commandMappings += (content -> id)
      id
  }

  private var lastDocumentID = 0

  var lastCommands : Seq[Command] = Seq()

  def makeZanyDiff(a : Seq[Command], b : Seq[Command]) =
    PIDEDiff.makeZanyDiff(
        (a : Command, b : Command) => (a.id == b.id))(a, b).toList

  def defineDocument(document : Seq[String]) =
    SessionManager.executeWithSessionLock(session => {
      import XML.Encode._

      val commands = document.map(c => Command(
          requireID(c), Document.Node.Name("Placeholder.v"),
          List(), Command_Span.unparsed(c))).toList
      val diff = makeZanyDiff(lastCommands, commands)

      if (!diff.isEmpty) {
        val documentID = getNextID
        new ProtocolPunt("coq", session).update(
            lastDocumentID, documentID,
            List(
              (Document.Node.Name("Placeholder.v"),
               Document.Node.Edits[(Option[Command], Option[Command]),
                 Command.Perspective](diff))))

        lastDocumentID = documentID
        lastCommands = commands
      }
    })

  private var commandMappings : Map[String, Int] = Map()
}
object DispatchPool {
  private val syntax = new Coq_Syntax

  private var nextID = 100
  def getNextID() = try nextID finally nextID += 1

  private def define_command(content : String) =
    SessionManager.executeWithSessionLock(session => {
      println(syntax.parse_spans(content))
      new ProtocolPunt("coq", session).define_command(
          Command(
              nextID,
              Document.Node.Name("Placeholder.v"),
              List(),
              Command_Span.unparsed(content)))
      getNextID
    })
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