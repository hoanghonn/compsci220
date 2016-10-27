import hw.tictactoe._

class Game (turn : Player , dim : Int , board : Map [( Int , Int ) , Player ]) extends GameLike [ Game ] {

	def isFinished (): Boolean = {
		if(board.size == dim*dim){
			true
		} else {
			if(getWinner() == Some(X) || getWinner == Some(O)) true
			else false
		}
	}

	//helper methods
	def checkPlayerFromPos(t : (Int,Int)): Option[Player] = {
		if (board.get(t) == Some(X)) Some(X)
		else if(board.get(t) == Some(O)) Some(O)
		else None
	}

	def checkList(a : List[Option[Player]]) : Option[Player] = {
		a match {
			case Nil => a(0)
			case h :: Nil => a(0)
			case h :: t => if(h == t(0)) checkList(t)
							else None
		}
	}

	//get horizontal to check 
	def getHorizontalHelper(a : (Int, Int)) : List[Option[Player]] = {
		if(a._2 < dim) checkPlayerFromPos(a._1,a._2) :: getHorizontalHelper(a._1, a._2+1)
		else Nil
	}

	def getHorizontal(a: (Int, Int)) : List[Option[Player]] = {
		if(a._1 < dim) checkList(getHorizontalHelper(a)) :: getHorizontal((a._1+1,a._2))
		else Nil
	}

	//get vertical to check
	def getVerticalHelper(a : (Int, Int)) : List[Option[Player]] = {
		if(a._1 < dim) checkPlayerFromPos(a._1,a._2) :: getHorizontalHelper(a._1+1, a._2)
		else Nil
	}

	def getVertical(a: (Int, Int)) : List[Option[Player]] = {
		if(a._2 < dim) checkList(getVerticalHelper(a)) :: getVertical((a._1,a._2+1))
		else Nil
	}

	//get diagonal to check
	def getDiagonalHelper(a : (Int, Int)) : List[Option[Player]] = {
		if(a._1 < dim && a._2 < dim) checkPlayerFromPos(a._1,a._2) :: getHorizontalHelper(a._1+1, a._2+1)
		else Nil
	}

	def getDiagonalHelper2(a : (Int, Int)) : List[Option[Player]] = {
		if(a._1 < dim && a._2 >= 0) checkPlayerFromPos(a._1,a._2) :: getDiagonalHelper2(a._1+1, a._2-1)
		else Nil
	}

	def getDiagonal(a: (Int, Int), b: (Int, Int)) : List[Option[Player]] = {
		List(checkList(getDiagonalHelper(a)), checkList(getDiagonalHelper2(b)))
		
	}

	//true if x won the match
	def xWin() : Boolean = {
		if(getHorizontal(0,0).contains(Some(X)) ||
		 getVertical(0,0).contains(Some(X)) || getDiagonal((0,0),(0,dim-1)).contains(Some(X))) true
		else false
	}

	//true if o won the match
	def oWin() : Boolean = {
		if(getHorizontal(0,0).contains(Some(O)) ||
		 getVertical(0,0).contains(Some(O)) || getDiagonal((0,0),(0,dim-1)).contains(Some(O))) true
		else false
	}

	/* Assume that isFinished is true */
	def getWinner(): Option [ Player ] = {
		if(xWin()) Some(X)
		else if (oWin()) Some(O)
		else None
	}

	def listOfMovesHelper1(num : (Int, Int)) : List[(Int,Int)] = {
		if(num._2 < dim){
			(num._1,num._2) :: listOfMovesHelper1((num._1,num._2+1))
		} else Nil
	}

	def listOfMoves(num : (Int, Int)) : List[(Int,Int)] = {
		if(num._1 < dim){
			listOfMovesHelper1(num) ::: listOfMoves((num._1+1,num._2))
		} else Nil
	}

	def compareList(l1: List[(Int,Int)], l2: List[(Int,Int)]) : List[(Int,Int)] = {
		l1 match {
			case Nil => Nil
			case h :: t => if(!l2.contains(h)) h :: compareList(t,l2)
							else compareList(t,l2) 
		}
	}

	def nextBoardsHelper(list : List[(Int,Int)]) : List[ Game] = {
		list match {
			case Nil => Nil
			case h :: t => if (turn == X) new Game(O,dim,board.updated(h,O)) :: nextBoardsHelper(t)
							else new Game(X,dim,board.updated(h,X)) :: nextBoardsHelper(t)
		}					
	}

	def nextBoards(): List [ Game ] = {
		val list = compareList(listOfMoves(0,0),board.keySet.toList)
		nextBoardsHelper(list)
	}

	def getPlayer() : Player = {
		turn
	}

	def getOtherPlayer() : Player = {
		if(turn == O) X
		else O
	}
}

object Solution extends MinimaxLike {
	type T = Game // T is an " abstract type member " of MinimaxLike

	def createGame ( turn : Player , dim : Int , board : Map [( Int , Int ) , Player ]): Game = {
		new Game(turn,dim,board)
	}

	def minimaxHelper(list : List[Game]) : List[Int] = {
		list match {
			case Nil => Nil
			case h :: t => if(h.getWinner == Some((h.getPlayer))) -1 :: minimaxHelper(t)
							else if(h.nextBoards == List()) 0 :: minimaxHelper(t)
							else minimaxHelper2(h.nextBoards) ::: minimaxHelper(t)
		}
	} 

	def minimaxHelper2(list : List [Game]) : List[Int] = {
		list match {
			case Nil => Nil
			case h :: t => if(h.getWinner == Some((h.getPlayer))) 1 :: minimaxHelper2(t)
							else if(h.nextBoards == List()) 0 :: minimaxHelper2(t)
							else minimaxHelper(h.nextBoards) ::: minimaxHelper2(t)
		}
	}

	def check(list : List[Int], board : Game) : Option [Player] = {
		def checkIfLost(a : Int) : Boolean = {
			if(a == -1) true
			else false
		}

		if(list.forall(num => checkIfLost(num))) Some(board.getOtherPlayer)
		else if(list.contains(0) && list.sum > 0) Some(board.getPlayer)
		else None
	}

	def minimax ( board : Game ): Option [ Player ] = {
		check(minimaxHelper(board.nextBoards), board)
 	}
}
