name := "solid-client"

version := "0.1"

scalaVersion := "2.12.3"


resolvers += "bblfish-snapshots" at "http://bblfish.net/work/repo/snapshots/"

//published at bblfish-snapshots
libraryDependencies ++= Seq(
   "org.w3"%%"banana-jena"%"0.8.5-SNAPSHOT"
)

//http://akka.io
libraryDependencies ++= Seq(
   "com.typesafe.akka" %% "akka-http" % "10.0.9",
   "com.typesafe.akka" %% "akka-http-testkit" % "10.0.9" % Test
)

//published at bblfish-snapshots
//https://github.com/read-write-web/akka-http-signature
libraryDependencies ++= Seq(
   "run.cosy" %% "akka-http-signature" % "0.2-SNAPSHOT"
)

//sbt -Dbanana.publish=bblfish.net:/home/hjs/htdocs/work/repo/
//sbt -Dbanana.publish=bintray
lazy val publicationSettings = pomSettings ++ {
   val pubre = """([^:]+):([^:]+)""".r
   Option(System.getProperty("banana.publish")) match {
      case Some("bintray") | None => Seq(
         // removed due to issue https://github.com/typesafehub/dbuild/issues/158
         //      publishTo := {
         //        val nexus = "https://oss.sonatype.org/"
         //        if (isSnapshot.value)
         //          Some("snapshots" at nexus + "content/repositories/snapshots")
         //        else
         //          Some("releases" at nexus + "service/local/staging/deploy/maven2")
         //      },
         //      releasePublishArtifactsAction := PgpKeys.publishSigned.value,
         //      publishArtifact in Test := false
      )
      case Some(pubre(host, path)) =>
         Seq(
            publishTo := Some(
               Resolver.ssh("banana.publish specified server",
                  host,
                  path + {
                     if (isSnapshot.value) "snapshots" else "releases"
                  }
               )
            ),
            publishArtifact in Test := false
         )
      case other => Seq()
   }
}

lazy val pomSettings = Seq(
   pomIncludeRepository := { _ => false},
   pomExtra :=
    <url>https://github.com/read-write-web/akka-http-signature</url>
     <developers>
        <developer>
           <id>bblfish</id>
           <name>Henry Story</name>
           <url>http://bblfish.net/</url>
        </developer>
     </developers>
     <scm>
        <url>git@github.com:read-write-web/akka-http-signature.git</url>
        <connection>scm:git:git@github.com:read-write-web/akka-http-signature.git</connection>
     </scm>
   ,
   licenses +=("Apache", url("http://www.apache.org/licenses/LICENSE-2.0"))
)

lazy val commonSettings = publicationSettings  ++ Seq(
   name := "solid-client",
   organization := "run.cosy",
   scalaVersion := "2.12.2",
   startYear := Some(2017)
)

commonSettings