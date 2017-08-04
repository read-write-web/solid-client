package run.cosy.solid.util

import akka.http.scaladsl.model.Uri
import org.w3.banana.{RDF, RDFOps}

class AkkaUriW(val uri: Uri) extends AnyVal  {
      def fragmentLess: Uri =
         if (uri.fragment.isEmpty) uri else uri.copy(fragment=None)
      
      def toRdf[Rdf<:RDF](implicit ops: RDFOps[Rdf]): Rdf#URI = ops.URI(uri.toString)
}
