// import Solution._
// import hw.tictactoe._

// class TestSuite extends org.scalatest.FunSuite {
	

// 	test (" test 1 "){
// 		assert(createGame(O, 3, Map((0,0)-> X, (2,2)-> O)).isFinished == false)
// 	}

// 	test (" test 2 "){
// 		assert(createGame(O, 3, Map((0,0)-> X, (2,2)-> O)).getWinner == None)
// 	}

// 	test (" test 3 "){
// 		assert(createGame(X, 3, Map((0,0)-> X, (0,1)->O, (0,2)->X , (2,2)-> O)).getWinner == None)
// 	}

// 	test (" test 4 "){
// 		assert(createGame(X, 3, Map((0,0)-> X, (0,1)->O, (0,2)->X , (2,2)-> O)).checkPlayerFromPos(0,0) == Some(X))
// 	}

// 	test (" test 5 "){
// 		assert(createGame(X, 3, Map((0,0)-> X, (0,1)->X, (0,2)->X , (2,2)-> O)).isFinished == true)
// 	}

// 	test (" test 6 : " + createGame(X,3,Map((0,0)->X,(0,1)->O,(0,2)->X)).nextBoards){
// 		assert( true )
// 	}
// }


import Solution._
import hw.tictactoe._

class TestSuite extends org.scalatest.FunSuite { 
		val a = new Game(X, 3, Map ((0, 0) -> X, (1, 0) -> O, (2, 0) -> X,
									(0, 1) -> X, (1, 1) -> O, /*(1, 2) -> X,*/
									(0, 2) -> O /*(2, 1) -> O, (2, 2) -> O)*/)) // draw

		val b = new Game(X, 3, Map ((0, 0) -> X/*, (1, 0) -> X, (2, 0) -> O*/,
									(0, 1) -> O, (1, 1) -> O/*, (2, 1) -> X*/,
									(0, 2) -> X, /*(1, 2) -> X, */(2, 2) -> O))  // draw

		val c = new Game(X, 4, Map ((0, 0) -> X/*, (1, 0) -> X*/, (2, 0) -> O,
									(0, 1) -> X, /*(1, 1) -> O, (2, 1) -> X,*/
									(0, 2) -> O, (1, 2) -> X,/* */(2, 2) -> O,
									/*(0, 3) -> X, (1, 2) -> X, */(3, 3) -> O)) //  draw

		val d = new Game(X, 3, Map ((0, 0) -> X,/* (1, 0) -> X, (2, 0) -> O,*/
									(0, 1) -> O, (1, 1) -> O, /*(2, 1) -> X,*/
									(0, 2) -> X, /*(1, 2) -> X, */(2, 2) -> O)) // draw

		val e = new Game(O, 3, Map ()) 											// draw

		val f = new Game(O, 3, Map ((0, 0) -> X, (1, 0) -> O,/* (2, 0) -> O,*/
									(0, 1) -> X, (1, 1) -> X, /*(2, 1) -> X,
									(0, 2) -> O,*/ /*(1, 2) -> X, */(2, 2) -> O)) // Some (X)
		val g = new Game(O, 3, Map ((1, 0) -> X,/* (1, 0) -> X, (2, 0) -> O,
									(0, 1) -> O, (1, 1) -> O,*/ /*(2, 1) -> X,*/
									(1, 2) -> X, /*(1, 2) -> X, */(2, 2) -> O)) // Same(O)	

		val k = new Game(O, 3, Map ((0, 0) -> X,/* (1, 0) -> X, (2, 0) -> O,
									(0, 1) -> O, (1, 1) -> O,*/ /*(2, 1) -> X,*/
									(0, 2) -> X, /*(1, 2) -> X, */(2, 2) -> O)) // Some(X)
	test("A: ") {
		assert(minimax(a)==None)

	}
	test("B: ") {
		assert(minimax(b)==None)

	}
	test("C: ") {
		assert(minimax(c)==None)

	}
	test("D: ") {
		assert(minimax(d)==None)

	}
	test("E: ") {
		assert(minimax(e)==None)
	}

	test("F: ") {
		assert(minimax(f)== Some(X))
	}

	test("G: ") {
		assert(minimax(g)== Some(O))
	}

	test("K: ") {
		assert(minimax(k)== Some(X))
	}
}