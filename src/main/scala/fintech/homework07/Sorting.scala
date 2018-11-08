package fintech.homework07

/**
  * Реализовать алгоритмы quick-sort и merge-sort
  *  используя *подходящие* *мутабельные* коллекции
  */

object Sorting {
  import scala.collection.mutable.ArrayBuffer
  import scala.reflect.ClassTag

  def mergeSort[T: ClassTag](arr: ArrayBuffer[T])(implicit ord: Ordering[T]): Unit = {
    def sort(left: Int, right: Int): Unit = {
      // merge я решил поместить на ещё один вложенный уровень в sort, чтобы избежать
      // ненужной, по сути, передачи аргументов left и right
      def merge(middle: Int): Unit = {
        var resIt = 0
        var leftIt = 0
        var rightIt = 0

        val leftLength = middle - left
        val rightLength = right - middle
        val result: Array[T] = new Array[T](right - left)

        while (leftIt < leftLength && rightIt < rightLength) {
          result(resIt) =
            if (ord.compare(arr(left + leftIt), arr(middle + rightIt)) < 0) {
              leftIt += 1
              arr(left + leftIt - 1)
            } else {
              rightIt += 1
              arr(middle + rightIt - 1)
            }
          resIt += 1
        }

        while (leftIt < leftLength) {
          result(leftIt + rightIt) = arr(left + leftIt)
          leftIt += 1
        }

        while (rightIt < rightLength) {
          result(leftIt + rightIt) = arr(middle + rightIt)
          rightIt += 1
        }

        for (i <- 0 until (leftIt + rightIt))
          arr(left + i) = result(i)
      }

      if (left + 1 < right) {
        val middle = (left + right) / 2

        sort(left, middle)
        sort(middle, right)
        merge(middle)
      }
    }

    if (arr.length > 1)
      sort(0, arr.length)
  }

//  def quickSort(???)

}
