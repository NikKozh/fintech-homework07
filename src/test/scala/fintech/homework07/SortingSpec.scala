package fintech.homework07

import org.scalatest.{FlatSpec, Matchers}

class SortingSpec extends FlatSpec with Matchers {
  import scala.collection.mutable.ArrayBuffer
  import scala.util.Random

  behavior of "Merge sort"

  it should "correctly sort small unsorted collections" in {
    val collection = ArrayBuffer(5, -3, 8, 4)
    Sorting.mergeSort(collection)
    collection should be(ArrayBuffer(-3, 4, 5, 8))
  }

  it should "correctly sort big unsorted collections" in {
    val collection = ArrayBuffer.fill(10000)(Random.nextInt())
    val forScalaSort = collection.clone
    Sorting.mergeSort(collection)
    collection should be(forScalaSort.sorted)
  }

  it should "correctly process already sorted collections" in {
    val collection = ArrayBuffer(-1, 2, 3, 4, 5)
    Sorting.mergeSort(collection)
    collection should be(ArrayBuffer(-1, 2, 3, 4, 5))
  }

  it should "correctly process empty collections" in {
    val collection = ArrayBuffer.empty[Int]
    Sorting.mergeSort(collection)
    collection should be(Nil)
  }

  behavior of "Quick sort"

  it should "correctly sort small unsorted collections" in {
    val collection = ArrayBuffer(5, -3, 8, 4)
    Sorting.quickSort(collection)
    collection should be(ArrayBuffer(-3, 4, 5, 8))
  }

  it should "correctly sort big unsorted collections" in {
    val collection = ArrayBuffer.fill(10000)(Random.nextInt())
    val forScalaSort = collection.clone
    Sorting.quickSort(collection)
    collection should be(forScalaSort.sorted)
  }

  it should "correctly process already sorted collections" in {
    val collection = ArrayBuffer(-1, 2, 3, 4, 5)
    Sorting.quickSort(collection)
    collection should be(ArrayBuffer(-1, 2, 3, 4, 5))
  }

  it should "correctly process empty collections" in {
    val collection = ArrayBuffer.empty[Int]
    Sorting.quickSort(collection)
    collection should be(Nil)
  }
}