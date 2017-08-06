package run.cosy.solid

import akka.http.scaladsl.model.headers.{ModeledCustomHeader, ModeledCustomHeaderCompanion}
import org.w3.banana.io._
import org.w3.banana.{JsonLDReaderModule, NTriplesReaderModule, RDF, RDFModule, RDFXMLReaderModule, TurtleReaderModule}
import run.cosy.solid.client.{MissingParserException, ParseException, ResponseSummary}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import scala.util.control.NoStackTrace

object RdfMediaTypes {
   import akka.http.scaladsl.model
   import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, PredefinedFromEntityUnmarshallers}
   import model.{ContentType, MediaTypes}
   import model.HttpCharsets._
   import model.MediaType.{applicationWithFixedCharset, applicationWithOpenCharset, text}

   import scala.util.Failure
   
   case class NoUnmarshallerException(mime: ContentType, msg: String) extends java.lang.RuntimeException with NoStackTrace with Product with Serializable
   
   //todo: check if there are other older mime types, or if there are widely used extensions
   val `text/turtle` = text("turtle","ttl")
   val `application/rdf+xml` = applicationWithOpenCharset("rdf+xml","rdf")
   val `application/ntriples` = applicationWithFixedCharset("ntriples",`UTF-8`,"nt")
   val `application/ld+json` = applicationWithOpenCharset("ld+json","jsonld")
   val `text/html` = MediaTypes.`text/html`
   
   
   def rdfUnmarshaller[R<:RDF](response: ResponseSummary)(implicit
    ec: ExecutionContext,
    rdfxmlReader: RDFReader[R, Try, RDFXML],
    turtleReader: RDFReader[R, Try, Turtle],
    ntriplesReader: RDFReader[R, Try, NTriples],
    jsonLdReader: RDFReader[R, Try, JsonLd]
//  rdfa: RDFReader[R, Try, RDFaXHTML]
   ): FromEntityUnmarshaller[R#Graph] = {
      //importing all readers this way is one way to go, but makes it difficult to integrate
      //with frameworks that may have missing ones
      PredefinedFromEntityUnmarshallers.stringUnmarshaller flatMapWithInput { (entity, string) ⇒
         //todo: use non blocking parsers
         val readerOpt = entity.contentType.mediaType match { //<- this needs to be tuned!
            case `text/turtle` => Some(turtleReader)
            case `application/rdf+xml` => Some(rdfxmlReader)
            case `application/ntriples` => Some(ntriplesReader)
            case `application/ld+json` => Some(jsonLdReader)
            // case `text/html` => Some(rdfa)
            case _ => None
         }
         readerOpt.map { reader =>
            Future.fromTry {
               reader.read(new java.io.StringReader(string), response.on.toString) recoverWith {
                  case e => Failure(
                     ParseException(response, string.take(400), e)
                  )
               }
            }
         } getOrElse {
            scala.concurrent.Future.failed(
               MissingParserException(response, string.take(400))
            )
         }
      }
   }
   
}

final class Slug(name: String) extends ModeledCustomHeader[Slug] {
   override def renderInRequests = true
   override def renderInResponses = false
   override val companion = Slug
   override def value: String = name
}
object Slug extends ModeledCustomHeaderCompanion[Slug] {
   override val name = "Slug"
   override def parse(value: String) = Try(new Slug(value))
}