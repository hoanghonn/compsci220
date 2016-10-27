import FunctionalDataStructures._

class TestSuite extends org.scalatest.FunSuite { 

	def fromList[A](alist : List[A]) : JoinList[A] = alist match {
		case Nil => Empty()
		case List(x) => Singleton(x)
		case _ => {
			val len = alist.length
			val (lhs, rhs) = alist.splitAt(len / 2)
			Join(fromList(lhs), fromList(rhs), len)
		}
	}

	def toList[A](alist : JoinList[A]): List[A] = alist match {
		case Empty() => Nil
		case Singleton(x) => List(x)
		case Join(alist1, alist2, _) => toList(alist1) ++ toList(alist2)
	}

	val joinList1 = Join(Singleton(1), Empty(), 1)
	val joinList2 = Join(Join(Singleton(4), Singleton(5), 2), Join(Singleton(2), Singleton(3), 2), 4)
	val joinList3 = Join(Join(Singleton(4), Empty(), 1), Join(Singleton(2), Singleton(3), 2), 3)
	val joinList4 = Join(Singleton(2), Singleton(3),2)
	val joinList5 = Join(Join(Join(Singleton(1), Empty(), 1), Empty(), 1), Join(Singleton(3), Singleton(4), 2), 3)
	val joinList6 = Join(Empty(), Join(Singleton(4), Singleton(5), 2), 2)

	test("enqueue method 1: ") {
		assert(enqueue(5, Queue(List(1,2), List(4,3))) == Queue(List(1,2), List(5,4,3)))
	}

	test("dequeue method 1: ") {
		assert(dequeue(Queue(List(1,2), List(4,3))) == Some(1,Queue(List(2), List(4,3))))
	}

	test("dequeue method 2: ") {
		assert(dequeue(Queue(List(), List())) == None)
	}

	test("dequeue method 3: ") {
		assert(dequeue(Queue(List(), List(4,3))) == Some(3,Queue(List(4), List())))
	}

	test("dequeue method 4: ") {
		assert(dequeue(Queue(List(1,2), List())) == Some(1,Queue(List(2), List())))
	}

	def compareOp(a : Int, b : Int) : Boolean = {
		if (a > b) true
		else false
	}

	test("max method 1: ") {
		assert(max(Empty(),compareOp) == None)
	}

	test("max method 2: ") {
		assert(max(joinList1,compareOp) == Some(1))
	}

	test("max method 3: ") {
		assert(max(joinList2,compareOp) == Some(5))
	}

	test("max method 4: ") {
		assert(max(joinList5,compareOp) == Some(4))
	}

	test("first method 1: ") {
		assert(first(joinList1) == Some(1))
	}

	test("first method 2: ") {
		assert(first(joinList2) == Some(4))
	}

	test("first method 3: ") {
		assert(first(joinList6) == Some(4))
	}

	test("rest method 1: ") {
		assert(rest(Join(Join(Singleton(5), Empty(),1), Join(Singleton(1), Singleton(2),2),3)) == 
			Some(Join(Empty(), Join(Singleton(1), Singleton(2),2),2)))
	}

	test("rest method 2: ") {
		assert(rest(joinList1) == Some(Empty()))
	}

	test("rest method 3: ") {
		assert(rest(joinList6) == Some(Join(Empty(), Join(Empty(), Singleton(5), 1), 1)))
	}

	test("rest method 4: ") {
		assert(rest(joinList5) == 
			Some(Join(Empty(), Join(Singleton(3), Singleton(4), 2), 2)))
	}

	test("nth method 1: ") {
		assert(nth(joinList1, 1) == None)
	}

	test("nth method 2: ") {
		assert(nth(joinList2, 1) == Some(5))
	}

	test("nth method 3: ") {
		assert(nth(joinList2, 3) == Some(3))
	}

	test("nth method 4: ") {
		assert(nth(joinList6, 0) == Some(4))
	}

	def f(a: Int) : String = {
		a.toString
	}

	test("map method 1: ") {
		assert(map(f, joinList1) == Join(Singleton("1"), Empty(), 1))
	}

	def pred(x: Int) : Boolean = {
		x%2 ==0
	}

	test("filter method 1: "){ 
		assert(toList(filter(pred, joinList1)) == toList(Empty()))
	}

	test("filter method 2: "){ 
		assert(toList(filter(pred, joinList4)) == toList(Join(Singleton(2), Empty(), 1)))
	}

	test("filter method 3: "){ 
		assert(toList(filter(pred, joinList4)) == toList(Join(Singleton(2), Empty(), 1)))
	}

	test("filter method 4: "){ 
		assert(toList(filter(pred, joinList6)) == toList(Join(Empty(), Join(Singleton(4), Empty(), 1), 1)))
	}
}