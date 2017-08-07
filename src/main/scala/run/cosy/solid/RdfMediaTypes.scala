package run.cosy.solid

import akka.http.scaladsl.model.MediaType
import akka.http.scaladsl.model.headers.{ModeledCustomHeader, ModeledCustomHeaderCompanion}
import org.w3.banana.io.{RDFXML, _}
import org.w3.banana.jena.Jena
import org.w3.banana.{JsonLDReaderModule, NTriplesReaderModule, RDF, RDFModule, RDFXMLReaderModule, TurtleReaderModule}
import run.cosy.solid.client.{MissingParserException, ParseException, ResponseSummary}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import scala.util.control.NoStackTrace

sealed trait RdfMediaTypes[+T,Rdf<:RDF] {
   type Banana
   def akka: MediaType.WithOpenCharset
   def reader: RDFReader[Rdf,Try,Banana]
   def writer: RDFWriter[Rdf,Try,Banana]
}


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
   type `text/turtle` = `text/turtle`.type
   val `application/rdf+xml` = applicationWithOpenCharset("rdf+xml","rdf")
   type `application/rdf+xml` = `application/rdf+xml`.type
   val `application/ntriples` = applicationWithOpenCharset("ntriples","nt")    //<- may want this to be fixed to utf-8 according to specs...
   type `application/ntriples` = `application/ntriples`.type
   val `application/ld+json` = applicationWithOpenCharset("ld+json","jsonld")
   type `application/ld+json` = `application/ld+json`.type
   val `text/html` = MediaTypes.`text/html`
   
   implicit val turtleWitness: RdfMediaTypes[`text/turtle`, Jena] = new RdfMediaTypes[`text/turtle`,Jena] {
      override def akka = `text/turtle`
      override type Banana = Turtle
      override def reader = Jena.turtleReader
      override def writer = Jena.turtleWriter
   }
   
   implicit val rdfxmlWitness: RdfMediaTypes[`application/rdf+xml`,Jena] =
      new RdfMediaTypes[`application/rdf+xml`,Jena] {
         override def akka = `application/rdf+xml`
         override type Banana = RDFXML
         override def reader = Jena.rdfXMLReader
         override def writer = Jena.rdfXMLWriter
      }
   
   implicit val ntriplesWitness: RdfMediaTypes[`application/ntriples`,Jena] =
      new RdfMediaTypes[`application/ntriples`,Jena] {
         override def akka = `application/ntriples`
         override type Banana = NTriples
         override def reader = Jena.ntriplesReader
         override def writer = Jena.ntriplesWriter
      }
   implicit val jsonldWitness: RdfMediaTypes[`application/ld+json`,Jena] =
      new RdfMediaTypes[`application/ld+json`,Jena] {
         override def akka = `application/ld+json`
         override type Banana = org.w3.banana.io.JsonLd
   
         override def reader = Jena.jsonldReader
         override def writer = Jena.jsonldCompactedWriter
      }
   
   
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