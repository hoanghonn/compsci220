object FunctionalDataStructures{
	case class Queue[A](front : List[A], back : List[A])

	def enqueue[A](elt: A, q: Queue[A]) : Queue[A] = new Queue[A](q.front, elt :: q.back) 

	def dequeue[A](q : Queue[A]) : Option[(A, Queue[A])] = {
		q.front match {
			case Nil => q.back match {
				case Nil => None
				case h1 :: t1 => Some((q.back.reverse(0), Queue(q.back.reverse.drop(1), List())))
			}
			case h :: t => Some((h, Queue(t, q.back)))
		}
	}

	sealed trait JoinList[A] { 
		val size : Int
	}

	case class Empty[A]() extends JoinList[A] {
		val size = 0
	}

	case class Singleton[A](elt : A) extends JoinList[A] {
		val size = 1
	}

	case class Join[A](lst1 : JoinList[A], lst2 : JoinList[A], size : Int) extends JoinList[A]

	def max[A](lst : JoinList[A], compare: (A, A) => Boolean) : Option[A] = {
		lst match {
			case Empty() => None
			case Singleton(x) => Some(x)
			case Join(a, b, _) => (max(a, compare), max(b, compare)) match {
				case (Some(x), Some(y)) => if( compare(x, y) == true) { Some(x)}
											else { Some(y) }
				case (Some(x), None) => Some(x)
				case (None, Some(y)) => Some(y)
				case (None, None) => None

			}
		}
	}

	def first[A](lst : JoinList[A]) : Option[A] = {
		lst match {
			case Empty() => None
			case Singleton(x) => Some(x)
			case Join(alist1, alist2, _) => if(alist1.size >= 1) {first(alist1)}
											else { first(alist2)}

		}
	}

	def restOp[A](lst : JoinList[A]) : JoinList[A] = {
		lst match {
			case Empty() => Empty()
			case Singleton(x) => Empty()
			case Join(alist1, alist2, n) => if(alist1.size == 1) { if(alist2.size == 0) {Empty()} 
																	else { Join(Empty(), alist2, n-1)}}
											else if(alist1.size == 0) { Join(alist1, restOp(alist2),n-1) }
											else { Join(restOp(alist1), alist2, n-1)}
		}
	}

	def rest[A](lst : JoinList[A]) : Option[JoinList[A]] = {
		lst match { 
			case Empty() => None 
			case Singleton(x) => Some(Empty())
			case _ => Some(restOp(lst))
		}
	}

	def nth[A](lst: JoinList[A], n : Int) : Option[A] = {
		if(n < 0 && n > lst.size -1) { None }
		else {
			lst match {
				case Empty() => None
				case Singleton(x) => Some(x)
				case Join(alist1, alist2, a) => if(n == 0) { first(lst)}
												else {nth(rest(lst).get,n-1)} 
			}
		}
	}

	def map[A,B](f: A => B, lst: JoinList[A]) : JoinList[B] = {
		lst match {
			case Empty() => Empty()
			case Singleton(x) => Singleton(f(x))
			case Join(alist1, alist2, n) => Join(map(f, alist1), map(f, alist2), n)
		}
	}

	def filter[A](pred : A => Boolean, lst : JoinList[A]) : JoinList[A] = {
		lst match {
			case Empty() => Empty()
			case Singleton(x) => if(pred(x)) { Singleton(x)}
								else Empty()	
			case Join(alist1, alist2, n) => 
				val joinsize = filter(pred, alist1).size + filter(pred, alist2).size
				if(joinsize == 0) Empty()
				else if(joinsize == 1) { if(alist1.size == 0) filter(pred,alist2) else filter(pred,alist1)}
				else{
					Join(filter(pred,alist1), filter(pred,alist2), alist1.size + alist2.size)
				}



				// (filter(pred,alist1), filter(pred,alist2)) match {
				// case (Empty(),Empty()) => Join(filter(pred,alist1), filter(pred,alist2),n-2)
				// case (Singleton(x),Empty()) => Join(filter(pred,alist1), filter(pred,alist2), n-1)
				// case (Empty(),Singleton(x)) => Join(filter(pred,alist1), filter(pred,alist2), n-1)
				// case (Singleton(x), Singleton(y)) => Join(filter(pred,alist1), filter(pred,alist2), n)
				// case (l1,l2) => Join(filter(pred,alist1), filter(pred,alist2), alist1.size + alist2.size)
				// }
		}
	}

} 