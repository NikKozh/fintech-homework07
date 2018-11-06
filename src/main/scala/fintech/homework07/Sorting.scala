package fintech.homework07

/**
  * Реализовать алгоритмы quick-sort и merge-sort
  *  использую *подходящие* *мутабельные* коллекции
  */

object Sorting {
  object MergeSort {
    /*
    * Обоснование выбора коллекций:
    *
    * В методах используются две коллекции, одна в аргументе и одна как временный буфер.
    * Для обеих коллекций используются только две операции: доступ и обновление.
    * Среди мутабельных реализаций есть несколько таких, у которых эти операции выполняются
    * за константное время: ArrayBuffer, StringBuilder, ArraySeq, ArrayStack и Array.
    *
    * Для внутреннего временного буфера, который никогда не виден пользователю,
    * я решил использовать plain Array из Java, т.к. я не пользуюсь в данном случае никакими
    * методами Scala и, соответственно, никаких обёрток не создаётся. Из минусов, которые
    * я обнаружил - необходимость ClassTag, как следствие, если пользователь захочет сортировать
    * массив со своими классами, ему помимо Ordering ещё придётся позаботиться и о ClassTag.
    * Возможно, из-за этого стоит использовать другую коллекцию?
    *
    * Для аргумента сортировки я выбрал ArrayBuffer, т.к. у него есть prepend и append,
    * и он может быть более удобен вызывающему сортировку пользователю.
    *
    * Таким образом, все отдельные операции внутри методов выполняются за константное время,
    * общая сложность сортировки по времени должна быть O(n*log(n)).
    * */

    import scala.collection.mutable.ArrayBuffer
    import scala.reflect.ClassTag
    
    def mergeSort[T: ClassTag](arr: ArrayBuffer[T])(implicit ord: Ordering[T]): Unit = {
      def innerMergeSort(arr: ArrayBuffer[T], left: Int, right: Int): Unit = {
        if (left + 1 < right) {
          val middle = (left + right) / 2

          innerMergeSort(arr, left, middle)
          innerMergeSort(arr, middle, right)
          merge(arr, left, middle, right)
        }
      }

      if (arr.length > 1)
        innerMergeSort(arr, 0, arr.length)
    }

    /*
    * Решил реализовать эту сортировку через работу с одним массивом и несколькими индексами
    * вместо разделения самого массива на части, чтобы не множить сущности понапрасну и не
    * создавать лишних коллекций, если уж работаем с мутабельностью.
    * */
    private def merge[T: ClassTag](arr: ArrayBuffer[T], left: Int, middle: Int, right: Int)
                          (implicit ord: Ordering[T]): Unit = {
      var resIt   = 0
      var leftIt  = 0
      var rightIt = 0
      
      val leftLength  = middle - left
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
  }

//  def quickSort(???)

}
