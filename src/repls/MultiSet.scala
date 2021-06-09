package repls

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class MultiSet[T]() {

  private var set: Seq[T] = _
  private val multiplicity: mutable.Map[T, Int] = mutable.Map[T, Int]()

  private def createSetFromMultiplicity(newMultiplicityMap: Map[T, Int]): MultiSet[T] = {
    val newSet = ArrayBuffer[T]()
    newMultiplicityMap foreach {
      case (k, v) => {
        for (_ <- 1 to v) newSet.append(k)
      }
    }
    MultiSet(newSet.toSeq)
  }

  def *(that: MultiSet[T]): MultiSet[T] = {
    val newMultiplicityMap = this.multiplicity map {
      case (k: T, v: Int) => (k, Math.min(v, that.multiplicity.getOrElse(k, 0)))
    }
    createSetFromMultiplicity(newMultiplicityMap.toMap)
  }

  def +(that: MultiSet[T]): MultiSet[T] = {
    val newSet: Seq[T] = this.set ++ that.set
    MultiSet(newSet)
  }

  def -(that: MultiSet[T]): MultiSet[T] = {
    val newMultiplicityMap = this.multiplicity map {
      case (k: T, v: Int) =>
        val newMultiplicity = v - that.multiplicity.getOrElse(k, 0)
        if (newMultiplicity < 0) (k, 0)
        else (k, newMultiplicity)
    }
    createSetFromMultiplicity(newMultiplicityMap.toMap)
  }

  def toSeq: Seq[T] = {
    set
  }

  override def toString: String = {
    s"{${set.map(_.toString).sorted.mkString(",")}}"
  }
}

object MultiSet {

  def apply[T](elements: Seq[T]): MultiSet[T] = {
    val m = MultiSet[T]()
    m.set = elements
    m.set foreach { k: T =>
      m.multiplicity.put(k, m.multiplicity.getOrElse(k, 0) + 1)
    }
    m
  }
}
