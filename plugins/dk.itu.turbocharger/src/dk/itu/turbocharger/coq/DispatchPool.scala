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

  def defineDocument(document : Seq[String]) =
    SessionManager.executeWithSessionLock(session => {
      import XML.Encode._

      val commands = document.map(c => requireID(c))

      val nextDocumentID = getNextID
      new ProtocolPunt("coq", session).update(
          lastDocumentID, nextDocumentID, List())

      println(document.mkString("\n"))
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