import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "authApp"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      "org.scalaz" %% "scalaz-core" % "6.0.4",
      "net.tanesha.recaptcha4j" % "recaptcha4j" % "0.0.7", //recaptcha
      "net.sf.opencsv" % "opencsv" % "2.1",
      "jp.t2v" % "play20.auth_2.9.1" % "0.3",
      "com.typesafe" %% "play-plugins-mailer" % "2.0.4",
      "se.radley" %% "play-plugins-salat" % "1.1",
      "org.mindrot" % "jbcrypt" % "0.3m"
    )

    val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
      routesImport += "se.radley.plugin.salat.Binders._",
      templatesImport += "models._",
      templatesImport += "authentication._",
      templatesImport += "viewImplicits._",
      templatesImport += "org.bson.types.ObjectId",

      resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/",
      resolvers += "Typesafe Snapshots" at "http://repo.typesafe.com/typesafe/snapshots/",
      resolvers += Resolver.url("FireMuzzy GitHub Play Repository", url("http://firemuzzy.github.com/releases/"))(Resolver.ivyStylePatterns),
      resolvers += "jbcrypt repo" at "http://mvnrepository.com/"
    )
}

