package com.satish.examples.asynctesting

import scala.concurrent.{Await, ExecutionContext, Future}
import cats.Applicative
import cats.syntax.applicative.*
import cats.instances.list.*
import cats.instances.future.*
import cats.Traverse
import cats.syntax.traverse.*

import java.util.concurrent.Executors
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

trait UptimeClient[F[_]]:
  def uptime(host: String): F[Int]

object MyClient extends UptimeClient[Future]:
  override def uptime(host: String): Future[Int] = Future(host.length * 3)

class UptimeService[F[_]: Applicative](client: UptimeClient[F]):
  def hostsUptime(hosts: List[String]): F[List[Int]] =
    hosts.traverse(client.uptime)


object PingCalculator extends App:

  given exCtx: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  val hosts = List(
    "http://myowg.com",
    "www.google.com",
    "https://example.co.in"
  )

  val service = new UptimeService[Future](MyClient)

  val pings : Future[List[Int]] = service.hostsUptime(hosts)

  println(Await.result(pings, 1.second))