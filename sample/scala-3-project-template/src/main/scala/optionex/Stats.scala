package optionex

object Stats:

  def mean(ls : Seq[Double]) : Option[Double] = ls match {
    case Nil => None
    case _ => Some(ls.sum / ls.length)
  }

  def variance(ls : Seq[Double]) : Option[Double] =
    mean(ls) flatMap (m => mean(ls.map(l => math.pow(l - m, 2))))

  def variance2(ls : Seq[Double]) : Option[Double] =
    for{
      seqMean <- mean(ls)
      m <- mean(ls.map(l => math.pow(l - seqMean, 2)))
    }yield m


