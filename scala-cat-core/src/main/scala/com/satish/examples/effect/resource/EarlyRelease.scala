package com.satish.examples.effect.resource

import cats.effect.{ExitCode, IO, IOApp, Resource}
import com.satish.examples.effect.pareffects.Debug.*

import scala.io.Source

case class Config(url: String)

object Config:
  def fromSource(s : Source): IO[Config] =
    for {
      config <- IO(Config(s.getLines().next()))
      _ <- IO(s"$config is read").debug
    } yield config

  def configResource(url: String) : Resource[IO,Config] =
     Resource.eval(sourceResource(url).use(s => fromSource(s)))

  def sourceResource(s: String) : Resource[IO, Source] =
    Resource.make(
      IO(s"Opening source - $s").debug *> IO(Source.fromString(s))
    )(_ => IO(s"closing source - $s").debug.void)

trait DbConnection:
  def execute(query: String) : IO[String]

class FakeConnection extends DbConnection:
  override def execute(query: String): IO[String] =
    IO(s"result for $query")

object DbConnection:
  def make(url: String): Resource[IO, DbConnection] =
    Resource.make(
      IO(s"making connection to $url").debug *> IO(new FakeConnection)
    )(_ => IO(s"closing connection to $url").debug.void )


object EarlyRelease extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    val dbConnection : Resource[IO, DbConnection] = for{
      cfg <- Config.configResource("www.secret.db")
      db <- DbConnection.make(cfg.url)
    } yield db
    dbConnection.use(conn => conn.execute("select * from Person")).as(ExitCode.Success)





