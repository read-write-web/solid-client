package run.cosy.crypto

import java.math.BigInteger
import java.security.KeyFactory
import java.security.interfaces.RSAPublicKey
import java.security.spec.RSAPublicKeySpec

import akka.http.scaladsl.model.Uri
import org.w3.banana.{PointedGraph, RDF, RDFOps, binder}
import org.w3.banana.binder.{PGBinder, RecordBinder, ToPG}

object Cert {
   def binderWithName[Rdf<:RDF](u: Uri)(implicit ops: RDFOps[Rdf]): PGBinder[Rdf, RSAPublicKey] =
      new Cert[Rdf].binderRootName(u.toString())
   
   def binder[Rdf<:RDF](implicit ops: RDFOps[Rdf]): PGBinder[Rdf, RSAPublicKey] =  new Cert[Rdf].binder

}

class Cert[Rdf<:RDF](implicit ops: RDFOps[Rdf]) {
   import org.w3.banana.{CertPrefix, RDF, RDFOps, binder}
   implicit val recordBinder = org.w3.banana.binder.RecordBinder[Rdf](ops)
   val cert = CertPrefix[Rdf]
   import org.w3.banana.binder._
   import recordBinder._

   implicit val rsaClassUri = recordBinder.classUrisFor[RSAPublicKey](cert.RSAPublicKey)
   val factory = KeyFactory.getInstance("RSA")
   val exponent = property[BigInteger](cert.exponent)
   val modulus = property[Array[Byte]](cert.modulus)
   
   val binder: PGBinder[Rdf, RSAPublicKey] =
      pgb[RSAPublicKey](modulus, exponent)(
         (m: Array[Byte], e: BigInteger) => factory.generatePublic(new RSAPublicKeySpec(new BigInteger(m), e)).asInstanceOf[RSAPublicKey],
         (key: RSAPublicKey) => Some((key.getModulus.toByteArray, key.getPublicExponent))
      ).withClasses(rsaClassUri)
   
   def binderRootName(uri: String)  =
      pgbWithConstId[RSAPublicKey](uri)(modulus, exponent)(
         (m: Array[Byte], e: BigInteger) => factory.generatePublic(new RSAPublicKeySpec(new BigInteger(m), e)).asInstanceOf[RSAPublicKey],
         (key: RSAPublicKey) => Some((key.getModulus.toByteArray, key.getPublicExponent))
      ).withClasses(rsaClassUri)
}