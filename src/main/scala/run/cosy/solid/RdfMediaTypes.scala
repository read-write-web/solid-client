package run.cosy.solid

import org.w3.banana.jena.Jena.{Rdf, jsonldReader, ntriplesReader, rdfXMLReader, turtleReader}
import run.cosy.solid.client.{MissingParserException, ParseException, ResponseSummary}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NoStackTrace

object RdfMediaTypes {
   import akka.http.scaladsl.model
   import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, PredefinedFromEntityUnmarshallers}
   import model.ContentType
   import model.HttpCharsets._
   import model.MediaType.{applicationWithFixedCharset, applicationWithOpenCharset, text}

   import scala.util.Failure
   
   case class NoUnmarshallerException(mime: ContentType, msg: String) extends java.lang.RuntimeException with NoStackTrace with Product with Serializable
   
   //todo: check if there are other older mime types, or if there are widely used extensions
   val `text/turtle` = text("turtle","ttl")
   val `application/rdf+xml` = applicationWithOpenCharset("rdf+xml","rdf")
   val `application/ntriples` = applicationWithFixedCharset("ntriples",`UTF-8`,"nt")
   val `application/ld+json` = applicationWithOpenCharset("ld+json","jsonld")
   
   
   def rdfUnmarshaller(response: ResponseSummary)(
    implicit ec: ExecutionContext
   ): FromEntityUnmarshaller[Rdf#Graph] =
      PredefinedFromEntityUnmarshallers.stringUnmarshaller flatMapWithInput { (entity, string) ⇒
         //todo: use non blocking parsers
         val readerOpt = entity.contentType.mediaType match { //<- this needs to be tuned!
            case `text/turtle` => Some(turtleReader)
            case `application/rdf+xml` => Some(rdfXMLReader)
            case `application/ntriples` => Some(ntriplesReader)
            case `application/ld+json` => Some(jsonldReader)
            // case `text/html` => new SesameRDFaReader()
            case _ => None
         }
         readerOpt.map{ reader=>
            Future.fromTry {
               reader.read(new java.io.StringReader(string),response.on.toString) recoverWith {
                  case e => Failure(
                     ParseException(response,string.take(400),e)
                  )}
            }
         } getOrElse {
            scala.concurrent.Future.failed(
               MissingParserException(response,string.take(400))
            )
         }
      }
   
}
