
 /*** P01 (*) Find the last element of a list.
  Example:
    scala> last(List(1, 1, 2, 3, 5, 8))
  res0: Int = 8
   */

  def last[T]( ls:List[T]):T = {ls.last}

  val ls1 = List(1, 1, 2, 3, 5, 8)
  println(ls1.last)


/*** P02 (*) Find the last but one element of a list.
Example:
scala> penultimate(List(1, 1, 2, 3, 5, 8))
res0: Int = 5
  */

  def penultimate[T]( ls:List[T]):T = {
    ls(ls.length - 2 )}

   val ls2 = List(1, 1, 2, 3, 5, 8)
  println(penultimate(ls2))

  def nth[T]( n:Int, ls:List[T]):T = {
    ls(n)
  }
  /***
    * P03 (*) Find the Kth element of a list.
    * By convention, the first element in the list is element 0.
    * Example:
    *
    * scala> nth(2, List(1, 1, 2, 3, 5, 8))
    * res0: Int = 2
    */
  val ls3 = List(1, 1, 2, 3, 5, 8)
  println(nth(2,ls3))




