object HOF {
	def map2[A,B,C](f : (A,B) => C, alist1 : List[A], alist2 : List [B]): List [C] = {
		alist1 match {
			case Nil => Nil
			case h1 :: t1 => alist2 match { 
				case Nil => Nil
				case h2 :: t2 => f(h1,h2) :: map2[A,B,C](f,t1,t2)
			}
		}
	}

	def zip[A,B](alist1 : List[A], alist2 : List[B]): List[(A,B)] = {
		alist1 match { 
			case Nil => Nil 
			case h1 :: t1 => alist2 match {
				case Nil => Nil 
				case h2 :: t2 => (h1, h2) :: zip[A,B](t1, t2)
			}
		}
	}

	def append[A](alist1 : List[A], alist2 : List[A]) : List[A] = {
		alist1 match {
			case Nil => alist2
			case h :: t => h :: (append[A](t,alist2))
		}
	}

	def flatten[A](alist : List[List[A]]): List [A] = {
		alist match { 
			case Nil => Nil
			case h :: t => append[A](h,flatten[A](t))
		}
	}..

	def flatten3[A](lst : List[List[List[A]]]): List[A] = {
		lst match { 
			case Nil => Nil
			case h :: t => h match { 
				case h:List[List[A]] => append[A](flatten[A](h),flatten3[A](t))
				// case h:List[A] => append[A](h,flatten3[A](t))
				// case h:A => h :: flatten3[A](t)
			}
		}
	}

	def buildList[A]( length : Int, f: Int => A ): List[A] = {
		if(length == 0) { Nil }
		else {
			append[A](buildList[A](length-1,f),List[A](f(length-1)))
		}
	}
	
	def mapList[A,B]( lst : List[A], f: A => List[B]): List [B] = {
		lst match { 
			case Nil => Nil 
			case h :: t => append[B](f(h), mapList[A,B](t,f))
		}
	}
	
	def isEven (x: Int ): Boolean = x % 2 == 0

	def trueList[A]( f : A => Boolean, lst : List[A]) : List[A] = {
		lst match { 
			case Nil => Nil 
			case h :: t => f(h) match {
				case true => h :: trueList[A](f,t)
				case false => trueList[A](f,t)
			}
		}
	}

	def falseList[A]( f : A => Boolean, lst : List[A]) : List[A] = {
		lst match { 
			case Nil => Nil 
			case h :: t => f(h) match {
				case true => falseList[A](f,t)
				case false => h :: falseList[A](f,t)
			}
		}
	}

	def partition[A]( f : A => Boolean, lst : List[A]): (List[A], List[A]) = (trueList[A](f,lst), falseList[A](f,lst))
	
	def lt(x : Int, y: Int) : Boolean = x < y

	def merge[A]( lessThan : (A, A) => Boolean , alist1 : List[A], alist2 : List[A]): List[A] = {
		alist1 match { 
			case Nil => alist2
			case h1 :: t1 => alist2 match {
				case Nil => Nil
				case h2 :: t2 => if(lessThan(h1,h2)) h2 :: merge[A](lessThan,alist1,t2) 
									else h1 :: merge[A](lessThan,t1,alist2)
			}
		}

	}

	def length[A](alist : List[A]) : Int = {
		alist match {
			case Nil => 0 
			case h :: t => 1 + length[A](t)
		}
	}

	def insertOrdered[A](lessThan : (A,A) => Boolean, n : A, lst : List[A]) : List[A] = {
		lst match{
			case Nil => List(n)
			case h :: t => { 
				if (lessThan(h,n)) { n :: h :: t }
				else { h :: insertOrdered[A](lessThan, n,t) }
			}
		}
	}
	
	def sort[A]( lessThan : (A, A) => Boolean , alist : List[A]): List[A] = {
		alist match{
			case Nil => Nil
			case h :: t => insertOrdered[A](lessThan, h,sort[A](lessThan,t))
		}
		
	}
}