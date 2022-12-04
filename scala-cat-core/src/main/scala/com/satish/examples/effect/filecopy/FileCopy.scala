package com.satish.examples.effect.filecopy

import java.io.{File, FileInputStream, FileOutputStream, InputStream, OutputStream}
import cats.effect.{ExitCode, IO, IOApp, Resource, Sync, MonadCancel}
import cats.syntax.all.*

object FileCopy:

  // [F[_] : Sync] --> means (using S : Sync[F])
  //Sync[F] is invoking apply method and just providing the implicit S in scope.
  def inputResource[F[_] : Sync](file: File): Resource[F, InputStream] =
    Resource.make{
      Sync[F].blocking(new FileInputStream(file))
    }{
      fis => Sync[F].blocking(fis.close).handleErrorWith(_ => Sync[F].unit)
    }

  def outputResource[F[_] : Sync](file: File): Resource[F, OutputStream] =
    Resource.make{
      Sync[F].blocking(new FileOutputStream(file))
    }{
      fis => Sync[F].blocking(fis.close).handleErrorWith(_ => Sync[F].unit)
    }

  def inputOutputResource[F[_] : Sync](in: File, out: File) : Resource[F, (InputStream, OutputStream)] =
    for{
      fin <- inputResource(in)
      fout <- outputResource(out)
    } yield (fin, fout)

  def copy[F[_] : Sync](src: File, dest: File) : F[Long] =
    inputOutputResource(src, dest).use{
      (fin, fout) => transfer(fin, fout, new Array[Byte](100 * 10), 0)
    }

  def transfer[F[_] : Sync](in : InputStream, out: OutputStream, buffer : Array[Byte], acc: Long) : F[Long] =
    for{
      n <- Sync[F].blocking{
          println(s"[${Thread.currentThread().getName}] - executing file copy")
          in.read(buffer, 0, buffer.length)
      }
      count <- if n == -1 then Sync[F].pure(acc) else Sync[F].blocking(out.write(buffer, 0, n)) *> transfer(in, out, buffer, acc + n)
    }yield count


object FileCopyMain extends IOApp:
  import FileCopy.*
  override def run(args: List[String]): IO[ExitCode] =
    val input = new File("/Users/satish.kumar1/tmp/file.txt")
    val output = new File("/Users/satish.kumar1/tmp/notes.txt")

    (for{
      n <- copy[IO](input, output)
      _ <- IO.println(s"Copied ${n} Bytes from ${input} to ${output} file")
    }yield n).as(ExitCode.Success)





