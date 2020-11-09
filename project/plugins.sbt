resolvers ++= Seq(
  Resolver.bintrayIvyRepo("epfl-lara", "sbt-plugins"),
  Resolver.bintrayRepo("epfl-lara", "princess"),
)

val StainlessVersion = "0.7.4"

addSbtPlugin("ch.epfl.lara" % "sbt-stainless" % StainlessVersion)

