package run.cosy.solid.client




// import $file.RDFaBananaParser, RDFaBananaParser.{SesameRDFaReader,SesameRDFXMLReader}

//import $ivy.`ch.qos.logback:logback-classic:1.2.3`

import _root_.run.cosy.auth.{HttpSignature => Sig}
import _root_.run.cosy.solid.util._
import _root_.run.cosy.solid.{RdfMediaTypes, Slug}
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpEntity.Default
import akka.http.scaladsl.model.{Uri => AkkaUri, _}
import akka.http.scaladsl.unmarshalling.Unmarshaller
import akka.stream._
import akka.stream.scaladsl._
import akka.util.ByteString
import org.w3.banana._
import org.w3.banana.io.{RDFWriter, Turtle}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Try}


object Web {
   
   
   def rdfGetRequest(uri: AkkaUri): HttpRequest = {
      import RdfMediaTypes._
      import akka.http.scaladsl.model.headers.Accept
      HttpRequest(uri=uri.fragmentLess)
       .addHeader(Accept(`text/turtle`,`application/rdf+xml`,
          `application/ntriples`,
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

/** summary of a response for logging and debugging purposes */
case class ResponseSummary(
 on: AkkaUri, code: StatusCode,
 header: Seq[HttpHeader],
 respTp: ContentType,
 content: Future[String] = Future.failed(new Exception("not used"))
)

class Web[Rdf<:RDF](implicit
   val ec: ExecutionContext,
   val as: ActorSystem,
   val mat: Materializer
) {
   import Web._
   type PGWeb = Interpretation[PointedGraph[Rdf]]
   
   //see: https://github.com/akka/akka-http/issues/195
   /**
     * Act on the request by calling into the Web.
     * @param req completed HttpRequest
     * @param maxRedirect maximum number of redicrects
     * @param history of requests as response summaries
     * @param keyChain list of keys to be used for authentication
     * @return a successful Http Response and a history of responses that lead to it, or a Future.failure
     */
   def run(
    req: HttpRequest, maxRedirect: Int = 4,
    history: List[ResponseSummary]=List(),
    keyChain: List[Sig.Client]=List()
   ): Future[(HttpResponse,List[ResponseSummary])] = try {
      import StatusCodes.{Success, Redirection, Unauthorized}
      Http().singleRequest(req)
       .recoverWith{case e=>Future.failed(ConnectionException(req.uri.toString,e,history))}
       .flatMap { resp =>
          def summary = {
             ResponseSummary(
                req.uri,resp.status,resp.headers,
                resp.entity.contentType,
                resp.entity.dataBytes.take(1)
                 .map(_.decodeString(Unmarshaller.bestUnmarshallingCharsetFor(resp.entity).nioCharset))
                 .runFold("")((prev,str)=>prev++"chunk["+str+"]")
             )
          }
          resp.status match {
             case Success(_) => Future.successful((resp,summary::history))
             case Redirection(_) =>
                resp.header[headers.Location].map { loc =>
                   val newReq = req.copy(uri = loc.uri)
                   if (maxRedirect > 0)
                      run(newReq, maxRedirect - 1,summary::history)
                   else Http().singleRequest(newReq).map((_,summary::history))
                }.getOrElse(Future.failed(
                   HTTPException(summary,s"Location header not found on ${resp.status} for ${req.uri}",history)
                ))
             case Unauthorized  =>
                import akka.http.scaladsl.model.headers.{Date, `WWW-Authenticate`}
                val date = Date(akka.http.scaladsl.model.DateTime.now)
                val reqWithDate = req.addHeader(date)
                val tryFuture = for {
                   wwa <- resp.header[`WWW-Authenticate`]
                    .fold[Try[`WWW-Authenticate`]](
                      Failure(HTTPException(summary,"no WWW-Authenticate header",history))
                   )(scala.util.Success(_))
                   headers <- Try { Sig.Client.signatureHeaders(wwa).get } //<- this should always succeed
                   client <- keyChain.headOption.fold[Try[Sig.Client]](
                      Failure(AuthException(summary,"no client keys",history))
                   )(scala.util.Success(_))
                   authorization <- client.authorize(reqWithDate,headers)
                } yield {
                   run(reqWithDate.addHeader(authorization), maxRedirect, summary::history, keyChain.tail)
                }
                Future.fromTry(tryFuture).flatten
             case _ =>
                Future.failed(StatusCodeException(summary,history))
          }
       }
   } catch {
      case NonFatal(e) => Future.failed(ConnectionException(req.uri.toString,e,history))
   }
   
   def GETRdfDoc(uri: AkkaUri, maxRedirect: Int=4): Future[HttpResponse] = run(rdfGetRequest(uri),maxRedirect).map(_._1)
   
   
//   def GETrdf(uri: AkkaUri): Future[Interpretation[R#Graph]] = {
//      import akka.http.scaladsl.unmarshalling.Unmarshal
//
//      GETRdfDoc(uri).flatMap {
//         case HttpResponse(status,headers,entity,protocol) => {
//            implicit  val reqUnmarhaller = RdfMediaTypes.rdfUnmarshaller(
//               ResponseSummary(uri,status,headers,entity.contentType)
//            )
//            Unmarshal(entity).to[R#Graph].map {g =>
//               Interpretation[R#Graph](uri,status,headers,entity.contentType,g)
//            }
//         }
//      }
//   }
   
   
   def turtlePostRequest(container: AkkaUri, graph: Rdf#Graph, slug: Option[String]=None)(
    implicit writer: RDFWriter[Rdf, Try, Turtle]
   ): Try[HttpRequest] = { //not much reason why this should fail!
      writer.asString(graph,"").map { ttl =>
         HttpRequest(
            HttpMethods.POST,
            entity = Default(
               RdfMediaTypes.`text/turtle`.withCharset(HttpCharsets.`UTF-8`),
               ttl.length,
               Source.single(ByteString(ttl))),
            uri = container,
            headers = slug.toList.map(Slug(_))
         )
      }
   }
   
   def POSTRdfDoc(container: AkkaUri, graph: Rdf#Graph, maxRedirect: Int=4)(
     implicit writer: RDFWriter[Rdf, Try, Turtle]
   ): Future[HttpResponse] =
      Future.fromTry(turtlePostRequest(container,graph)).flatMap{ req=>
         run(req,maxRedirect).map(_._1)
      }
   
   //   def pointedGET(uri: AkkaUri): Future[PGWeb] =
   //   GETrdf(uri).map(_.map(PointedGraph[R](uri.toRdf,_)))
   
   
}
