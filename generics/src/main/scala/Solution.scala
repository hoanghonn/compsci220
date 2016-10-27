import hw.generics._

sealed trait BinTree [A] extends ListLike[A, BinTree[A]]
case class Node [A]( lhs : BinTree [A], value : A , rhs : BinTree [A ]) extends BinTree [A] {
	def cons(head : A) : BinTree[A] = {
		new Node[A] (Leaf(), head, new Node[A](lhs, value, rhs))
	}

	def head() : Option[A] = {
		lhs match{
			case Leaf() => Some(value)
			case Node(lhs1, value1, rhs1) => lhs.head()
		}
	}

	def isEmpty() : Boolean = {
		if(value == Nil) true
		else false
	}

	def tail() : Option[BinTree[A]] = {
		(lhs, rhs) match{
			case (Leaf(), Leaf()) => Some(Leaf())
			case (Leaf(), Node(_,_,_)) => Some(rhs)
			case (Node(_,_,_), _ ) => Some(new Node(lhs.tail().get, value, rhs))
		}
	}
}
case class Leaf [A]() extends BinTree [A] {
	def cons(head : A) : BinTree[A] = {
		new Node(Leaf(), head, Leaf())
	}

	def head() : Option[A] = {
		None
	}

	def isEmpty() : Boolean = {
		true
	}

	def tail() : Option[BinTree[A]] = {
		None
	}
}

class C1 extends T3[Int,Int,Int,String,String,String,Int] with T2[Int, Int, String, String] {
	def f(a: Int , b : Int ): Int = 0
	def g(c: String ): String = ""
	def h(d: String ): Int = 0
}
class C2 extends T3[Int, Int, Int, Int, Int, Int, Int] with T2[Int, Int, Int, Int] with T1[Int,Int]{
	def f(a: Int , b : Int ): Int = 0
	def g(c: Int ): Int = 0
	def h(d: Int ): Int = 0
}
class C3 [A ]( x : A )  extends T3[Int, A, Int, A, String, String, A]{
	def f(a: Int , b : A ): Int = 0
	def g(c: A ): String = ""
	def h(d: String ): A = x
}
class C4 [A ]( x : Int , y: C4 [A ]) extends T3[Int, C4[A], C4[A], Int, C4[A], C4[A], Int] with T1[Int, C4[A]]{
	def f(a: Int , b : C4 [A ]): C4 [A] = b
	def g(c: Int ): C4 [A] = y
	def h(d: C4 [ A ]): Int = x
}

object ListFunctions {
	def createEmpty[A, C <: hw.generics.ListLike[A,C]](empty : A, alist : C) : C = {
		if(alist.isEmpty) alist
		else createEmpty(empty, alist.tail.get)
	}

	def filterHelper[A, C <: hw.generics.ListLike[A,C]](f : A => Boolean, alist1 : C, alist2 : C) : C = {
		if(alist1.tail != None){
			if(f(alist1.head.get) == false) filterHelper(f, alist1.tail.get, alist2)
			else filterHelper(f,alist1.tail.get, alist2.cons(alist1.head.get))
		} else {
			alist2
		}
	}

	def reverse[A, C <: hw.generics.ListLike[A,C]](empty : A, alist1 : C, alist2 : C) : C = {
		if(alist1.tail != None){
			reverse(empty, alist1.tail.get, alist2.cons(alist1.head.get))
		} else {
			alist2
		}
	}

	def filter[A, C <: hw.generics.ListLike[A,C]](f : A => Boolean, alist : C) : C = {
		if(alist.isEmpty){
			alist
		}
		else {
			val emptyList = createEmpty(alist.head.get, alist)
			val temp = filterHelper(f, alist, emptyList)
			reverse(temp.head.get, temp, emptyList)
		}
	}

	def append[A, C <: hw.generics.ListLike[A,C]](alist1 : C, alist2 : C) : C = {
		if(alist1.isEmpty && alist2.isEmpty) alist1
		else if(alist1.isEmpty) alist2
		else if(alist2.isEmpty) alist1
		else {
			val emptyList = createEmpty(alist1.head.get, alist1)
			val temp = reverse(alist1.head.get, alist1, emptyList)
			val temp2 = reverse(alist1.head.get, alist2, temp)
			reverse(alist1.head.get, temp2, emptyList)
		}
	}

	def sortHelper[A <: hw.generics.Ordered[A], C <: hw.generics.ListLike[A,C]](x : A, alist : C, temp : C) : C = {
		if(alist.tail != None) {
			if(x.compare(alist.head.get) == LT || x.compare(alist.head.get) == EQ) append[A,C](reverse(x, temp, createEmpty(x, alist)), alist.cons(x))
			else{
				sortHelper(x, alist.tail.get, temp.cons(alist.head.get))
			}
		}
		else append[A,C](reverse(x, temp, createEmpty(x, alist)), alist.cons(x))
	}

	def sortHelper2[A <: hw.generics.Ordered[A], C <: hw.generics.ListLike[A,C]](empty : A, alist1 : C, alist2 : C) : C = {
		if(alist1.tail != None) {
			sortHelper2(empty, alist1.tail.get, sortHelper(alist1.head.get, alist2, createEmpty(alist1.head.get, alist2)))
		} else{ alist2
		}
	}

	def sort[A <: hw.generics.Ordered[A], C <: hw.generics.ListLike[A, C]](alist: C) : C = {
		if(alist.isEmpty) alist
		else {
			val emptyList = createEmpty(alist.head.get, alist)
			sortHelper2(alist.head.get, alist, emptyList)
		}
	}
	
}
