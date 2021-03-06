
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
   
 /***  
  (*) Modified run-length encoding.
Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
Example:

scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
*/
     def encodeModified[A](ls: List[A]): List[Any] =
    encode(ls) map { t => if (t._1 == 1) t._2 else t }
   
   
   /*** 
   P12 (**) Decode a run-length encoded list.
Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
Example:

scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

*/
   def decode[A](ls: List[(Int, A)]): List[A] =
  ls flatMap { e => List.fill(e._1) (e._2) }
   
   // P13 (**) Run-length encoding of a list (direct solution).
//     Implement the so-called run-length encoding data compression method
//     directly.  I.e. don't use other methods you've written (like P09's
//     pack); do all the work directly.
//
//     Example:
//     scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//     res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
   def encodeDirect[A](ls: List[A]): List[(Int, A)] =
    if (ls.isEmpty) Nil
    else {
      val (packed, next) = ls span { _ == ls.head }
      (packed.length, packed.head) :: encodeDirect(next)
    }
   
   
 /***
  * P14 (*) Duplicate the elements of a list.
Example:
scala> duplicate(List('a, 'b, 'c, 'c, 'd))
res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
  */


def duplicate[A](ls: List[A]): List[A] = {
  ls flatMap  { e => List(e,e)  }
}


/***
P15 (**) Duplicate the elements of a list a given number of times.
Example:
  scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
  */

def duplicateN[A](n: Int,ls: List[A]): List[A] = {
  ls flatMap  { e => List.fill(n)(e)  }
}

   
   /***
  P16 (**) Drop every Nth element from a list.
Example:
scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
  */

def drop[A](n:Int, ls: List[A]): List[A] = {

  val ls1 = ls.drop(n)
  val ls2 = ls.dropRight(ls.length - n +1)
  val ls3 = ls2 ++ ls1
   ls3 } 
   
   /***
drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

P17 (*) Split a list into two parts.
  The length of the first part is given. Use a Tuple for your result.
Example:

  scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  */
   
   def split[A](n:Int, ls: List[A]):(List[A],List[A]) = {

  val ls1 = ls.drop(n)
  val ls2 = ls.dropRight(ls.length - n)
  val ls3 = (ls2,ls1)
  ls3 }

}
   
P18 (**) Extract a slice from a list.
  Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list. Start counting the elements with 0.
Example:

  scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: List[Symbol] = List('d, 'e, 'f, 'g)
  */

def slice[A](a:Int, b:Int, ls: List[A]): List[A] = {

  val ls1 = ls.drop(a)
  val ls2 = ls1.dropRight(ls.length - b)
  ls2
}


/***
P19 (**) Rotate a list N places to the left.
  Examples:
  scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)

scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
  */


def rotate[A](n:Int, ls: List[A]):List[A] = {
  
  
var ls1 = ls.dropRight( ls.length - math.abs(n) )
  var ls2 = ls.drop(math.abs(n))
  if (n>0) {
  return ls2 ++ ls1 }
  else { var ls1 = ls.drop( ls.length -math.abs(n)  )
    var ls2 = ls.dropRight(math.abs(n) )
    return ls1 ++ ls2  }
    
  
}

/***
P20 (*) Remove the Kth element from a list.
Return the list and the removed element in a Tuple. Elements are numbered from 0.
Example:

  scala> removeAt(1, List('a, 'b, 'c, 'd))
res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
  */

def removeAt[A](n:Int, ls:List[A]):(List[A],A) = {
  val ls1 = ls.drop(n+1)
  val ls2 = ls.dropRight(ls.length - n )
  val ls3 = ls2 ++ ls1
  val r = ls(n)
  return (ls3,r)

  
}

/***
  * P21 (*) Insert an element at a given position into a list.
  * Example:
  * scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
  * res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
  */


def insertAt[A](e:A, n:Int, ls:List[A]):List[A] = {

  val ls1 = ls.drop(n)
  val ls2 = ls.dropRight(ls.length - n )
  val ls4 = List(e)
  val ls3 = ls2 ++ ls4 ++ ls1

  return ls3
  
}


/***
  P22 (*) Create a list containing all integers within a given range.
Example:
scala> range(4, 9)
res0: List[Int] = List(4, 5, 6, 7, 8, 9)
  */

def range(a:Int, b:Int):List[Int] = {

  val res = for ( n:Int <- a to b ) yield n
  res.toList
}


/***
P23 (**) Extract a given number of randomly selected elements from a list.
Example:
  scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
res0: List[Symbol] = List('e, 'd, 'a)
Hint: Use the solution to problem P20
  */

import P20.removeAt

def randomSelect1[A](n: Int, ls: List[A]): List[A] =
  if (n <= 0) Nil
  else {
    val (rest, e) = removeAt((new util.Random).nextInt(ls.length), ls)
    e :: randomSelect1(n - 1, rest)
  }

/***
P23 (**) Extract a given number of randomly selected elements from a list.
Example:
  scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
res0: List[Symbol] = List('e, 'd, 'a)
Hint: Use the solution to problem P20
  */

import P20.removeAt

def randomSelect1[A](n: Int, ls: List[A]): List[A] =
  if (n <= 0) Nil
  else {
    val (rest, e) = removeAt((new util.Random).nextInt(ls.length), ls)
    e :: randomSelect1(n - 1, rest)
  }


/***P24 (*) Lotto: Draw N different random numbers from the set 1..M.
  Example:
  scala> lotto(6, 49)
res0: List[Int] = List(23, 1, 17, 33, 21, 37)
  ***/

// (new util.Random).nextInt(ls.length)

def lotto(a:Int, b:Int):List[Int] = {


 var resultList = for ( a <- 1 to a )
   yield ((new util.Random).nextInt(b) )
  resultList.toList
  
}


/***P25 (*) Generate a random permutation of the elements of a list.
  Hint: Use the solution of problem P23.
Example:

  scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)
  */

def randomPermute[A](ls:List[A]):List[A] = {

  def removeAt[A](n: Int, ls: List[A]): (List[A], A) = {
    val ls1 = ls.drop(n + 1)
    val ls2 = ls.dropRight(ls.length - n)
    val ls3 = ls2 ++ ls1
    val r = ls(n)
    return (ls3, r)
  }

  def randomSelect1[A](n: Int, ls: List[A]): List[A] = {
    if (n <= 0) Nil
    else {
      val (rest, e) = removeAt((new util.Random).nextInt(ls.length), ls)
      e :: randomSelect1(n - 1, rest)
    }
  }

  var resultList = randomSelect1(ls.length, ls)
  return resultList
  
}

// P26 (**) Generate the combinations of K distinct objects chosen from the N
//          elements of a list.
//     In how many ways can a committee of 3 be chosen from a group of 12
//     people?  We all know that there are C(12,3) = 220 possibilities (C(N,K)
//     denotes the well-known binomial coefficient).  For pure mathematicians,
//     this result may be great.  But we want to really generate all the possibilities.
//
//     Example:
//     scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
//     res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...

object P26 {
  // flatMapSublists is like list.flatMap, but instead of passing each element
  // to the function, it passes successive sublists of L.
  def flatMapSublists[A,B](ls: List[A])(f: (List[A]) => List[B]): List[B] = 
    ls match {
      case Nil => Nil
      case sublist@(_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
    }

  def combinations[A](n: Int, ls: List[A]): List[List[A]] =
    if (n == 0) List(Nil)
    else flatMapSublists(ls) { sl =>
      combinations(n - 1, sl.tail) map {sl.head :: _}
    }
}

// P27 (**) Group the elements of a set into disjoint subsets.
//     a) In how many ways can a group of 9 people work in 3 disjoint subgroups
//        of 2, 3 and 4 persons?  Write a function that generates all the
//        possibilities.
//
//        Example:
//        scala> group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
//        res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David, Evi), List(Flip, Gary, Hugo, Ida)), ...
//
//     b) Generalize the above predicate in a way that we can specify a list
//        of group sizes and the predicate will return a list of groups.
//
//        Example:
//        scala> group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
//        res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David), List(Evi, Flip, Gary, Hugo, Ida)), ...
//
//     Note that we do not want permutations of the group members;
//     i.e. ((Aldo, Beat), ...) is the same solution as ((Beat, Aldo), ...).
//     However, we make a difference between ((Aldo, Beat), (Carla, David), ...)
//     and ((Carla, David), (Aldo, Beat), ...).
//
//     You may find more about this combinatorial problem in a good book on
//     discrete mathematics under the term "multinomial coefficients".

object P27 {
  import P26.combinations

  def group3[A](ls: List[A]): List[List[List[A]]] =
    for {
      a <- combinations(2, ls)
      noA = ls -- a
      b <- combinations(3, noA)
    } yield List(a, b, noA -- b)

  def group[A](ns: List[Int], ls: List[A]): List[List[List[A]]] = ns match {
    case Nil     => List(Nil)
    case n :: ns => combinations(n, ls) flatMap { c =>
      group(ns, ls -- c) map {c :: _}
    }
  }
}


// P28 (**) Sorting a list of lists according to length of sublists.
//     a) We suppose that a list contains elements that are lists themselves.
//        The objective is to sort the elements of the list according to their
//        length.  E.g. short lists first, longer lists later, or vice versa.
//
//     Example:
//     scala> lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
//     res0: List[List[Symbol]] = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))
//
//     b) Again, we suppose that a list contains elements that are lists
//        themselves.  But this time the objective is to sort the elements
//        according to their length frequency; i.e. in the default, sorting is
//        done ascendingly, lists with rare lengths are placed, others with a
//        more frequent length come later.
//
//     Example:
//     scala> lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
//     res1: List[List[Symbol]] = List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n))
//
//     Note that in the above example, the first two lists in the result have
//     length 4 and 1 and both lengths appear just once.  The third and fourth
//     lists have length 3 and there are two list of this length.  Finally, the
//     last three lists have length 2.  This is the most frequent length.

object P28 {
  import P10.encode

  def lsort[A](ls: List[List[A]]): List[List[A]] =
    ls sort { _.length < _.length }
  
  def lsortFreq[A](ls: List[List[A]]): List[List[A]] = {
    val freqs = Map(encode(ls map { _.length } sort { _ < _ }) map { _.swap }:_*)
    ls sort { (e1, e2) => freqs(e1.length) < freqs(e2.length) }
  }
}


