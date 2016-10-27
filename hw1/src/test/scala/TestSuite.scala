import Lists._

class TestSuite extends org.scalatest.FunSuite {
	test ("oddNumbers properly defined") {
		assert(oddNumbers == List(1,3,5))
	}

	// test method sumDouble
	test ("method sumDouble makes right result with positive test") {
		assert(sumDouble(List(1,2,4)) == 14)
	}

	test ("method sumDouble makes right result with negative test q") {
		assert(sumDouble(List(14,5,6,7,-124)) == -184)
	}

	//test method removeZeroes
	test ("method removeZeroes does remove zero test 1") {
		assert(removeZeroes(List(123,43,6,3,0,6,4,0,0,4)) == List(123,43,6,3,6,4,4))
	}

	test ("method removeZeroes does remove zero test 2") {
		assert(removeZeroes(List(0,0,0,0,0,1)) == List(1))
	}

	// test method countEvens
	test ("method countEvens have the right result") {
		assert(countEvens(List(2,4,6,4,5,6)) == 5)
	}

	// test method remove Alternating
	test ("method remove the Alternating with even string in the list") {
		assert(removeAlternating(List("a","b","c","d","e","f")) == List("a","c","e"))
	}

	test ("method remove the Alternating with odd string in the list") {
		assert(removeAlternating(List("a","b","c","d","e")) == List("a","c","e"))
	}	

	test ("method remove the Alternating with 1 string in the list") {
		assert(removeAlternating(List("a")) == List("a"))
	}


	//test method isAscending
	test ("method isAscending is true with no repeated numbers") {
		assert(isAscending(List(1,2,3,4,5,6,7,8)) == true)
	}

	test ("method isAscending is false with no repeated numbers") {
		assert(isAscending(List(1,2,4,3,5,6,7,8)) == false)
	}

	test ("method isAscending is true with repeated numbers") {
		assert(isAscending(List(1,2,2,2,2,4,5,6)) == true)
	}

	test ("method isAscending is false with repeated numbers") {
		assert(isAscending(List(1,3,3,3,3,3,2,2,2,2,6)) == false)
	}

	test ("method isAscending is true with all same numbers") {
		assert(isAscending(List(2,2,2,2,2,2)) == true)
	}

	//test method addSub
	test ("method addSub works with 2 elements") {
		assert(addSub(List(4,3)) == 1)
	}

	test ("method addSub works with 3 elements") {
		assert(addSub(List(4,3,2)) == 3)
	} 

	test ("method addSub works with 4 elements") {
		assert(addSub(List(4,3,2,1)) == 2)
	}

	//test method alternate
	test ("method alternate works with Lists of 2 elements") {
		assert(alternate(List(1,2),List(3,4)) == List(1,3,2,4))
	}


	test ("method alternate works with Lists of 1 element") {
		assert(alternate(List(1),List(3)) == List(1,3))
	}


	test ("method alternate works with Lists of n elements") {
		assert(alternate(List(1,2,3,4,5,6,7),List(3,4,5,6,7,8,9)) == List(1,3,2,4,3,5,4,6,5,7,6,8,7,9))
	}

	//test method fromTo
	test ("method fromTo returns the correct result") {
		assert(fromTo(2,3) == List(2))
	}

	test ("method fromTo returns the correct result 2") {
		assert(fromTo(2,10) == List(2,3,4,5,6,7,8,9))
	}

	//test method insertOrdered
	test ("method insertOrdered inputs the correct spot") {
		assert(insertOrdered(5, List(1, 3, 7, 9)) == List(1, 3, 5, 7, 9))
	}

	test ("method insertOrdered inputs the correct spot 2") {
		assert(insertOrdered(5, List(1,2,3,4)) == List(1,2,3,4,5))
	}

	test ("method insertOrdered inputs the correct spot 3") {
		assert(insertOrdered(1, List(2,3,4,5,6)) == List(1,2,3,4,5,6))
	}

	test ("method insertOrdered throws exception") {
		intercept[Exception] {
			insertOrdered(1,List(2,4,3))
		}
	}

	//test method sort
	test ("method sort return sorted list") {
		assert(sort(List(1, 8, 7, 6)) == List(1,6,7,8))
	}

	test ("method sort return sorted list 2") {
		assert(sort(List(2,4,3,2)) == List(2,2,3,4))
	}

	test ("method sort return sorted list 3") {
		assert(sort(List(1,0)) == List(0,1))
	}

	test ("method sort return sorted list 4") {
		assert(sort(List(1)) == List(1))
	}

}