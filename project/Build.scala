import sbt._
import Keys._
import play.Project._
//import com.typesafe.sbt.SbtStartScript._

object ApplicationBuild extends Build {
  val appName         = "ChordCorea"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    // Add your project dependencies here,
    jdbc,
    anorm,
    "org.scalaz" % "scalaz-core_2.10" % "7.0.3",
    "mysql" % "mysql-connector-java" % "5.1.18",
    "commons-codec" % "commons-codec" % "1.4",
    "commons-io" % "commons-io" % "2.4",
    "joda-time" % "joda-time" % "2.3"
    )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    // Add your own project settings here 
    //import models._ 
    initialCommands := """
      |// make app resources accessible
      |Thread.currentThread.setContextClassLoader(getClass.getClassLoader)
      |new play.core.StaticApplication(new java.io.File("."))
      |import models.music._
      """.stripMargin    
  )

}
