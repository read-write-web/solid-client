package run.cosy.solid.client

import org.w3.banana.jena.Jena.Rdf

import scala.util.control.NoStackTrace


trait WebException extends java.lang.RuntimeException with NoStackTrace with Product with Serializable

case class HTTPException(
 response: ResponseSummary, msg: String,
 history: List[ResponseSummary]=List()
) extends WebException

case class AuthException(
 response: ResponseSummary, msg: String,
 history: List[ResponseSummary]=List()
) extends WebException

case class StatusCodeException(
 response: ResponseSummary,
 history: List[ResponseSummary]=List()
) extends WebException

case class ConnectionException(
 resourceUri: String, e: Throwable,
 history: List[ResponseSummary]=List()
) extends WebException

case class NodeTranslationException(
 graphLoc: String, problemNode: Rdf#Node, e: Throwable,
 history: List[ResponseSummary]=List()
) extends WebException

case class MissingParserException(
 response: ResponseSummary,
 initialContent: String,
 history: List[ResponseSummary]=List()
) extends WebException

case class ParseException(
 response: ResponseSummary,
 initialContent: String,
 e: Throwable,
 history: List[ResponseSummary]=List()
) extends WebException

case class LogException(
 history: List[ResponseSummary],
 e: Throwable)
