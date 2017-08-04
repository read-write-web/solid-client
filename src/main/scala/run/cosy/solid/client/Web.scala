package run.cosy.solid.client


import org.w3.banana._
import org.w3.banana.jena.Jena._
import org.w3.banana.jena.Jena.ops._
import run.cosy.solid.RdfMediaTypes

import scala.concurrent.ExecutionContext

// import $file.RDFaBananaParser, RDFaBananaParser.{SesameRDFaReader,SesameRDFXMLReader}

//import $ivy.`ch.qos.logback:logback-classic:1.2.3`

import akka.NotUsed
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{Uri => AkkaUri, _}
import akka.stream._
import akka.stream.scaladsl._
import run.cosy.auth.{HttpSignature => Sig}

import scala.concurrent.Future
import scala.util.control.{NoStackTrace, NonFatal}
import scala.util.{Failure, Success, Try}


object Web {
   type PGWeb = Interpretation[PointedGraph[Rdf]]
   
   val foaf = FOAFPrefix[Rdf]
   val rdfs = RDFSPrefix[Rdf]
   
   implicit class UriW(val uri: AkkaUri)  extends AnyVal {
      def fragmentLess: AkkaUri =
         if (uri.fragment.isEmpty) uri else uri.copy(fragment=None)
      
      def toRdf: Rdf#URI = URI(uri.toString)
   }
   
   def rdfRequest(uri: AkkaUri): HttpRequest = {
      import akka.http.scaladsl.model.headers.Accept
      import run.cosy.solid.RdfMediaTypes._
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
      def map[O](f: I => O) = this.copy(content=f(content))
   }
   
   
   implicit class HttResPG(val h: PGWeb) extends AnyVal {
      def jump(rel: Rdf#URI)(implicit web: Web): List[Future[PGWeb]] =
         (h.content/rel).toList.map{ pg =>
            if (pg.pointer.isURI) try {
               web.pointedGET(AkkaUri(pg.pointer.toString))
            } catch {
               case NonFatal(e) => Future.failed(NodeTranslationException(h.origin.toString, pg.pointer,e))
            }
            else Future.successful(h.copy(content=pg))
         }
      
      // does it make sense to use Http().superPool(...) ?
      //def jumps(rel: Rdf#URI)(implicit web: Web): Stream[PGWeb] {
      //}
   }
   
   import scala.collection.immutable
   def uriSource(uris: AkkaUri*): Source[AkkaUri,NotUsed] =
      Source(immutable.Seq(uris:_*).to[collection.immutable.Iterable])
   
   //simple transformation of a Future into an always successful one - useful to avoid failed futures leading to closure of streams
   def neverFail[X](fut: Future[X])(implicit ec: ExecutionContext): Future[Try[X]] = fut.transform(Success(_))
   
   //flatten a flow of Flow[Future[X]] to a Flow[X]
   def flattenFutureFlow[X](n: Int=1): Flow[Future[X],X,_] = Flow[Future[X]].mapAsyncUnordered(n)(identity)
   
   /**
     * return an Akka Source of Try[IRepresentation[Rdf#PointedGraphs]] starting
     * from the me WebID, and including any relevant rdfs.seeAlso linked files.
     * The source is useful for finding all the linked to friends, including broken
     * links, with very simple explanations as to what went wrong accessing those
     * (hence the Try).
     */
   def foafKnowsSource(webid: AkkaUri)(implicit web: Web): Source[Try[PGWeb],_] = {
      import web._
      uriSource(webid)
       .mapAsync(1){uri => web.pointedGET(uri)}
       .via(addSeeAlso)
       .mapConcat{ //jump to remote foaf.knows
          case Success(pgweb) => pgweb.jump(foaf.knows).to[immutable.Iterable].map(neverFail(_))
          case failure => immutable.Iterable(Future.successful(failure))
       }.via(flattenFutureFlow(50))
   }
   
   //add any rdfs:seeAlso going out from a node to the stream, placing the pointer on the same point in the other graph
   def addSeeAlso(implicit web: Web, ec: ExecutionContext): Flow[PGWeb,Try[PGWeb],_] = Flow[PGWeb].mapConcat{ pgweb =>
      val seqFut:  immutable.Seq[Future[PGWeb]] = pgweb.jump(rdfs.seeAlso).to[immutable.Seq]
      //we want the see Also docs to be pointing to the same URI as the original pgweb
      val seeAlso = seqFut.map(fut => fut.map( _.map(pg=>PointedGraph[Rdf](pgweb.content.pointer,pg.graph) )))
      (seeAlso :+ Future.successful(pgweb)).map(neverFail(_))
   }.via(flattenFutureFlow(50))  // 50 is a bit arbitrary
   
   def filterForSuccess[X] = Flow[Try[X]].collect{ case Success(x) => x }
   
   def filterLinkedTo(rel: Rdf#URI, obj: Rdf#URI): Flow[PGWeb,PGWeb,_] =
      Flow[PGWeb].filter(htres => (htres.content/rel).exists(_.pointer == obj))
   
   def consciensciousFriends(me: AkkaUri)(implicit web: Web): Source[PGWeb,_] = {
      import web._
      foafKnowsSource(me).via(filterForSuccess)
       .via(addSeeAlso)
       .via(filterForSuccess)
       .via(filterLinkedTo(foaf.knows,me.toRdf))
   }
   
   /*
   def xxx() = {

      val sinkFold2 = Sink.fold[List[Try[Web.PGWeb]],
                                Try[Web.PGWeb]](List()){ case (l,t)=> t::l }
      sourceJumpTryKn.toMat(sinkFold2)(Keep.right)
   }
   */
   
}

/** summary of a response for logging and debugging purposes */
case class ResponseSummary(
 on: AkkaUri, code: StatusCode,
 header: Seq[HttpHeader], respTp: ContentType)

class Web(implicit val ec: ExecutionContext, val as: ActorSystem, val mat: Materializer) {
   import Web._
   
   
   def GETRdfDoc(uri: AkkaUri, maxRedirect: Int=4): Future[HttpResponse] = GET(rdfRequest(uri),maxRedirect).map(_._1)
   
   //todo: add something to the response re number of redirects
   //see: https://github.com/akka/akka-http/issues/195
   def GET(
    req: HttpRequest, maxRedirect: Int = 4,
    history: List[ResponseSummary]=List(),
    keyChain: List[Sig.Client]=List()
   ): Future[(HttpResponse,List[ResponseSummary])] = {
      try {
         import StatusCodes.{Success, _}
         Http().singleRequest(req)
          .recoverWith{case e=>Future.failed(ConnectionException(req.uri.toString,e))}
          .flatMap { resp =>
             def summary = ResponseSummary(req.uri,resp.status,resp.headers,resp.entity.contentType)
             resp.status match {
                case Success(_) => Future.successful((resp,summary::history))
                case Redirection(_) => {
                   resp.header[headers.Location].map { loc =>
                      val newReq = req.copy(uri = loc.uri)
                      resp.discardEntityBytes()
                      if (maxRedirect > 0)
                         GET(newReq, maxRedirect - 1,summary::history)
                      else Http().singleRequest(newReq).map((_,summary::history))
                   }.getOrElse(Future.failed(HTTPException(summary,s"Location header not found on ${resp.status} for ${req.uri}")))
                }
                case Unauthorized  => {
                   import akka.http.scaladsl.model.headers.{Date, `WWW-Authenticate`}
                   val date = Date(akka.http.scaladsl.model.DateTime.now)
                   val reqWithDate = req.addHeader(date)
                   val tryFuture = for {
                      wwa <- resp.header[`WWW-Authenticate`]
                       .fold[Try[`WWW-Authenticate`]](
                         Failure(HTTPException(summary,"no WWW-Authenticate header"))
                      )(scala.util.Success(_))
                      headers <- Try { Sig.Client.signatureHeaders(wwa).get } //<- this should always succeed
                      client <- keyChain.headOption.fold[Try[Sig.Client]](
                         Failure(AuthException(summary,"no client keys"))
                      )(scala.util.Success(_))
                      authorization <- client.authorize(reqWithDate,headers)
                   } yield {
                      GET(reqWithDate.addHeader(authorization), maxRedirect, summary::history, keyChain.tail)
                   }
                   Future.fromTry(tryFuture).flatten
                }
                case _ => {
                   resp.discardEntityBytes()
                   Future.failed(StatusCodeException(summary))
                }
             }
          }
      } catch {
         case NonFatal(e) => Future.failed(ConnectionException(req.uri.toString,e))
      }
   }
   
   
   
   def GETrdf(uri: AkkaUri): Future[Interpretation[Rdf#Graph]] = {
      import akka.http.scaladsl.unmarshalling.Unmarshal
      
      GETRdfDoc(uri).flatMap {
         case HttpResponse(status,headers,entity,protocol) => {
            implicit  val reqUnmarhaller = RdfMediaTypes.rdfUnmarshaller(
               ResponseSummary(uri,status,headers,entity.contentType)
            )
            Unmarshal(entity).to[Rdf#Graph].map {g =>
               Interpretation[Rdf#Graph](uri,status,headers,entity.contentType,g)
            }
         }
      }
   }
   
   def pointedGET(uri: AkkaUri): Future[PGWeb] =
      GETrdf(uri).map(_.map(PointedGraph[Rdf](uri.toRdf,_)))
   
   
}
