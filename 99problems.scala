
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

 /***
    * P04 (*) Find the number of elements of a list.
    * Example:
    * scala> length(List(1, 1, 2, 3, 5, 8))
    * res0: Int = 6
    */

// ls.length !! ??

  // Simple recursive solution.
  def lengthRecursive[A](ls: List[A]): Int = ls match {
    case Nil       => 0
    case _ :: tail => 1 + lengthRecursive(tail)
   
   
     P05 (*) Reverse a list.
  Example:
    scala> reverse(List(1, 1, 2, 3, 5, 8))
  res0: List[Int] = List(8, 5, 3, 2, 1, 1)
   */
   
   def reverse[T](ls:List[T]):List[T] = {
   ls.reverse }
   
   

   
   def isPalindrome[A](ls: List[A]): Boolean = ls == ls.reverse
   
   // P07 (**) Flatten a nested list structure.
//     Example:
//     scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
//     res0: List[Any] = List(1, 1, 2, 3, 5, 8)

object P07 {
  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case ms: List[_] => flatten(ms)
    case e => List(e)
  }
 // you are also matching on the elements type.
}

   
   /***
P08 (**) Eliminate consecutive duplicates of list elements.
  If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
  Example:

  scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
  */

// Standard recursive.
def compressRecursive[A](ls: List[A]): List[A] = ls match {
  case Nil       => Nil
  case h :: tail => h :: compressRecursive(tail.dropWhile(_ == h))
}


/***
  P09 (**) Pack consecutive duplicates of list elements
into sublists.
If a list contains repeated elements they should
be placed in separate sublists.
Example:

scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  */

object P09 {
  def pack[A](ls: List[A]): List[List[A]] = {
    if (ls.isEmpty) List(List())
    else {
      val (packed, next) = ls span { _ == ls.head }
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }
}
   
   /***
   // P10 (*) Run-length encoding of a list.
//     Use the result of problem P09 to implement the so-called run-length
//     encoding data compression method.  Consecutive duplicates of elements are
//     encoded as tuples (N, E) where N is the number of duplicates of the
//     element E.
//
//     Example:
//     scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//     res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
*/
  
     def encode[A](ls: List[A]): List[(Int, A)] = {


    var reslist = pack(ls)
    for (a <- reslist) yield (a.length, a.head)

  }
   
   /***
   or : object P10 {
  import P09.pack
  def encode[A](ls: List[A]): List[(Int, A)] =
    pack(ls) map { e => (e.length, e.head) }
}
*/
   
   
   
   
   
 


