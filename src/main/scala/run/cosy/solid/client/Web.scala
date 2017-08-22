package run.cosy.solid.client

// import $file.RDFaBananaParser, RDFaBananaParser.{SesameRDFaReader,SesameRDFXMLReader}

import _root_.run.cosy.auth.{HttpSignature => Sig}
import _root_.run.cosy.solid.util._
import _root_.run.cosy.solid.{RdfMediaTypes, Slug}
import akka.http.scaladsl.model.HttpEntity.Default
import akka.http.scaladsl.model.{Uri => AkkaUri, _}
import akka.stream.scaladsl._
import akka.util.ByteString
import org.w3.banana._
import org.w3.banana.io._

import scala.concurrent.Future
import scala.util.Try


object Web {
   
   def GETrdf(uri: AkkaUri): HttpRequest = {
      import RdfMediaTypes._
      import akka.http.scaladsl.model.headers.Accept
      HttpRequest(uri=uri.fragmentLess)
       .addHeader(Accept(`text/turtle`,`application/rdf+xml`,
          `application/n-triples`,
          `application/ld+json`.withQValue(0.8), //our parser uses more memory
          `text/html`.withQValue(0.2))) //we can't specify that we want RDFa in our markup
   }
   
   
   //interpreted HttpResponse
   case class Interpretation[I](origin: AkkaUri, status: StatusCode,
    headers: Seq[HttpHeader], fromContentType: ContentType,
    content: I) {
      def map[O](f: I => O): Interpretation[O] = this.copy(content=f(content))
   }
   
   
}

class Web[Rdf<:RDF](http: HTTP) {
   import Web._
   type PGWeb = Interpretation[PointedGraph[Rdf]]
   
   def GETpg(
    uri: AkkaUri,
    keyChain: List[Sig.Client]=List()
   )(implicit
    ops: RDFOps[Rdf],
    rdfxmlReader: RDFReader[Rdf, Try, RDFXML],
    turtleReader: RDFReader[Rdf, Try, Turtle],
    ntriplesReader: RDFReader[Rdf, Try, NTriples],
    jsonLdReader: RDFReader[Rdf, Try, JsonLd]
   ): Future[(PGWeb,List[ResponseSummary])] = {
      import akka.http.scaladsl.unmarshalling.Unmarshal
      import http.{ec,mat}

      http.run(GETrdf(uri),keyChain=keyChain).flatMap {
         case (HttpResponse(status,headers,entity,protocol),summary) =>
            implicit  val reqUnmarhaller = RdfMediaTypes.rdfUnmarshaller(
               ResponseSummary(uri,status,headers,entity.contentType)
            )
            Unmarshal(entity).to[Rdf#Graph].map { g =>
               (Interpretation[PointedGraph[Rdf]](
                  uri,
                  status,
                  headers, entity.contentType,
                  PointedGraph[Rdf](uri.toRdf,g)),summary)
            }
      }
   }
   
   
   def POST[M](container: AkkaUri, graph: Rdf#Graph,
      slug: Option[String]=None
   )(implicit
      mediaType: RdfMediaTypes[M,Rdf]
   ): Try[HttpRequest] = { //not much reason why this should fail!
      mediaType.writer.asString(graph,"").map { ttl =>
         val ttlBs = ByteString(ttl)
         HttpRequest(
            HttpMethods.POST,
            entity = Default(
               mediaType.akka.withCharset(HttpCharsets.`UTF-8`),
               ttlBs.length,
               Source.single(ttlBs)),
            uri = container,
            headers = slug.toList.map(Slug(_))
         )
      }
   }
   
   def PUT[M](resource: AkkaUri, graph: Rdf#Graph
   )(implicit
    mediaType: RdfMediaTypes[M,Rdf]
   ): Try[HttpRequest] = { //not much reason why this should fail!
      mediaType.writer.asString(graph,"").map { ttl =>
         HttpRequest(
            HttpMethods.PUT,
            entity = Default(
               mediaType.akka.withCharset(HttpCharsets.`UTF-8`),
               ttl.length,
               Source.single(ByteString(ttl))),
            uri = resource
         )
      }
   }
   
   def DELETE[M](resource: AkkaUri)(implicit
    mediaType: RdfMediaTypes[M,Rdf]
   ): HttpRequest = HttpRequest(
         HttpMethods.DELETE,
         uri = resource
      )
   
   /* note: the sparqlUpdate here is not type safe */
   def PATCHsparql[M](resource: AkkaUri, sparqlUpdate: String): HttpRequest = {
      val suBs = ByteString(sparqlUpdate)
      HttpRequest(
         HttpMethods.PATCH,
         uri = resource,
         entity = Default(
            RdfMediaTypes.`application/sparql-update`.withCharset(HttpCharsets.`UTF-8`),
            suBs.length,Source.single(suBs)
         )
      )
   }
   
}
