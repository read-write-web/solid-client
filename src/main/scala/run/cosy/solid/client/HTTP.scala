package run.cosy.solid.client

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentType, HttpHeader, StatusCode}

import scala.concurrent.{ExecutionContext, Future}
import akka.http.scaladsl.model.{Uri => AkkaUri, _}
import akka.http.scaladsl.unmarshalling.Unmarshaller
import akka.stream.Materializer

import scala.util.{Failure, Try}
import scala.util.control.NonFatal
import _root_.run.cosy.auth.{HttpSignature => Sig}




/** summary of a response for logging and debugging purposes */
case class ResponseSummary(
 on: AkkaUri, code: StatusCode,
 header: Seq[HttpHeader],
 respTp: ContentType,
 content: Future[String] = Future.failed(new Exception("not used"))
)


/** Wrap the akka http layer, with the ability to authenticate and follow redirects */
class HTTP(implicit
 val ec: ExecutionContext,
 val as: ActorSystem,
 val mat: Materializer
) {
   //see: https://github.com/akka/akka-http/issues/195
   /**
     * Act on the request by making the http call. This returns the final HTTP Response after
     * following a limited number of redirects and attempting to authenticate with elements
     * of the keychain if possible.
     *
     * todo:
     *   - the keychain should be a function that can evolve separately, allowing implementations that knows
     *   how to follow rel="acl" headers in order to find out if it is worth authenticating, and that would
     *   be able to mak decisions as to what key to use in its keychain. As such it has to break out of the
     *   HTTP level, and interpret responses to other requests.
     *
     * @param req completed HttpRequest
     * @param maxRedirect maximum number of redirects
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
}
