import HOF._

class TestSuite extends org.scalatest.FunSuite {
	test (" map2 with add"){
		def add(x : Int , y: Int ): Int = x + y
		assert( map2(add , List (1 , 2, 3) , List (4 , 5 , 6)) == List (5 , 7, 9))
	}

	test (" zip test 1"){
		assert ( zip ( List (1 , 2, 3) , List (4 , 5, 6)) == List ((1 ,4) , (2 , 5) , (3 , 6)))
	}
	test (" zip test 2"){
		assert( zip( List("George","Teddy"), List("Washington","Roosevelt")) ==
			List(("George","Washington"),("Teddy","Roosevelt")))
	}

	test (" flatten test"){
		assert( flatten( List( List(1,2), List(3,4))) == List(1,2,3,4))
	}	

	test (" flatten3 test"){
		assert( flatten3( List( List( List(1, 5, 6), List(9, 12)), List(List(16), List(20)))) == 
			List(1, 5, 6, 9, 12, 16, 20))
	}

	test (" buildList test"){
		def f(x: Int ) = x
		assert( buildList(10, f) == List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
	}

	test (" buildList test 1"){
		def f(x: Int ) = x
		assert( buildList(0, f) == List())
	}

	test (" mapList test"){
		def f(n: Int ): List[Int] = buildList(n, (_: Int) => n)
		assert( mapList( List(1, 2, 3), f) == List(1, 2, 2 , 3 , 3 , 3))
	}

	test (" mapList test 1" ){
		def f(n: Int ): List[Int] = buildList(n, (_: Int) => n)
		assert( mapList( List(1, 2, 3,4), f) == List(1, 2, 2 , 3 , 3 , 3,4, 4, 4,4))
	}

	test (" partition test 1"){
		assert( partition( isEven , List (1 ,2 ,3 ,4 ,5 ,6)) == ( List (2 ,4 ,6) , List (1 ,3 ,5)))
	}

	test (" partition test 2 ") {
		assert ( partition ( isEven , List (2 ,4 ,6)) == ( List (2 ,4 ,6) , Nil ))
	}
	test (" partition test 3 ") {
			assert ( partition ( isEven , List (1 ,3 ,5)) == ( Nil , List (1 ,3 ,5)))
	}

	test (" merge test 1 ") {
		assert ( merge (lt , List (5 , 3, 1) , List (10 , 6, 0)) == List (10 , 6, 5, 3, 1, 0))
	}

	test (" merge test 2 ") {
		assert ( merge (lt , List (3, 1) , List (6, 0)) == List (6, 3, 1, 0))
	}

	test (" sort test 1" ) {
		assert ( sort (lt , List (5 ,1 ,2 ,3 ,4 ,5)) == List (5 ,5 ,4 ,3 ,2 ,1))
	}

	test (" sort test 2" ) {
		assert ( sort (lt , List (3,4,5,6,2,3,1,0,9)) == List (9,6,5,4,3,3,2,1,0))
	}

	test (" sort test 3" ) {
		assert ( sort (lt , List (2,1)) == List (2,1))
	}
}
