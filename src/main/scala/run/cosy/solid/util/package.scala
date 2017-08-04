package run.cosy.solid

import akka.http.scaladsl.model.Uri

package object util {
   implicit def akkaUriW(uri: Uri) = new AkkaUriW(uri)
}
