package monoid

object MapMerge:
  def mapMergeMonoid[K,V](valMonoid : Monoid[V]): Monoid[Map[K,V]] = new:
    override def zero: Map[K,V] = Map.empty
    override def op(m1: Map[K,V], m2: Map[K,V]): Map[K,V] =
      (m1.keySet ++ m2.keySet).foldLeft(zero){
        (m, k) => m.updated(k, {
          valMonoid.op(m1.getOrElse(k, valMonoid.zero), m2.getOrElse(k, valMonoid.zero))
        })
      }

object MapMergeMain extends App:
  import MapMerge.mapMergeMonoid
  import monoid.Monoid.intAddition
  val baseMonoid: Monoid[Map[String,Int]] = mapMergeMonoid(intAddition)

  val octSalary = Map("mary" -> 100, "john" -> 200, "terry" -> 300)
  val novSalary = Map("mary" -> 50, "john" -> 75, "terry" -> 90, "newJohn" -> 23)

  val totalSalary = baseMonoid.op(octSalary, novSalary)
  println(totalSalary)

  val complexMonoid: Monoid[Map[String, Map[String,Int]]] = mapMergeMonoid(baseMonoid)


