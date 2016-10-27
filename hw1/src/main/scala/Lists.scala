object Lists {
	val oddNumbers = 1 :: 3 :: 5 :: Nil

	def sumDouble(n : List[Int]) : Int = {
		n match{
			case Nil => 0
			case h :: tail => 2 * h + sumDouble(tail)
		}
	}

	def removeZeroes(n : List[Int]) : List[Int] = {
		n match{
			case Nil => Nil
			case h :: t => if (h != 0) { h :: removeZeroes(t)} else { removeZeroes(t) }
		}
	}

	def countEvens(n : List[Int]) : Int = {
		n match{
			case Nil => 0
			case h :: t => {
				if (h % 2 == 0) { 1 + countEvens(t) }
				else { countEvens(t)}
			}
		}
	}

	 
	def removeAlternating(n : List[String]) : List[String] = { 
		n match {
			case Nil => Nil
			case h :: Nil => h :: Nil
			case h :: m :: t => h :: removeAlternating(t)
		}
	}
	

	def isAscending(n : List[Int]) : Boolean = { 
		n match{
			case Nil => true
			case h :: Nil => true 
			case h :: m :: t => {
				if (h <= m) { isAscending(m :: t) }
				else { false }
			}

		}

	}
	
	def addSub(n : List[Int]) : Int = {
		n match { 
			case Nil => 0 
			case h :: Nil => h
			case h :: m :: t => (h - m) + addSub(t)
		}
	}

	def alternate(n : List[Int], m : List[Int]) : List[Int] = {
		n match { 
			case h :: t => h :: alternate(m, t)
			case Nil => Nil

		}
	}

	def fromTo(m : Int, n : Int) : List[Int] = {
		if(m + 1 > n) { Nil }
		else { m :: fromTo(m+1,n) }
	}

	def insertOrdered(n : Int, lst : List[Int]) : List[Int] = {
		if(!isAscending(lst)) { throw new Exception("This list is not ascending") }
		lst match{
			case Nil => List(n)
			case h :: t => { 
				if (n <= h) { n :: h :: t }
				else { h :: insertOrdered(n,t) }
			}
		}
	}

	def sort(lst : List[Int]) : List[Int] = {
		lst match{
			case Nil => Nil
			case h :: t => insertOrdered(h,sort(t))
		}
	}


}