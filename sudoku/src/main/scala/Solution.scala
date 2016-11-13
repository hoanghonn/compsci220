import hw.sudoku._

object Solution extends SudokuLike {
	type T = Board

	def listOfAll(list : List[Int]) : List[(Int,Int)] = {
		list match {
			case Nil => Nil
			case h :: t => listOfRow(lst, h) ::: listOfAll(t)
		}}

	def parseHelper(str : String, list : List[(Int, Int)], board : Board) : Board = {
		list match {
			case Nil => board
			case h :: t => if(str.charAt(0) == '.') parseHelper(str.substring(1), t, board)
			 				else parseHelper(str.substring(1), t, board.place(h._1, h._2, str.charAt(0).toInt-48)) 
		}}

	def parse ( str : String ): Board = { 
		val emptyBoard = new Board(listOfAll(lst).map(x => x -> List(1,2,3,4,5,6,7,8,9)).toMap)
		parseHelper(str, listOfAll(lst), emptyBoard)
	}

	val lst = List(0,1,2,3,4,5,6,7,8)
	val list1 = List(0,1,2)
	val list2 = List(3,4,5)
	val list3 = List(6,7,8)

	def listOfRow(list : List[Int], row : Int) : List[(Int, Int)] = {
		list match {
			case Nil => Nil
			case h :: t => List((row, h)) ::: listOfRow(t, row)
		}}
	def listOfCol(list : List[Int], col : Int) : List[(Int, Int)] = {
		list match {
			case Nil => Nil
			case h :: t => List((h, col)) ::: listOfCol(t, col)
		}}
	def listChecker(num : Int) : List[Int] = {
		if(list1.contains(num)) list1
		else if(list2.contains(num)) list2
		else list3}
	def listOfBox(row : Int, col : Int) : List[(Int, Int)] = {
		val temp = listChecker(row)
		listOfRow(listChecker(col), temp(0)) ::: 
		listOfRow(listChecker(col), temp(1)) :::
		listOfRow(listChecker(col), temp(2))}

	// You can use a Set instead of a List (or , any Iterable )
	def peers(row : Int, col : Int): List[(Int, Int)] = {
		listOfRow(lst, row).filter(_ != (row, col)) ::: 
		listOfCol(lst,col).filter(_ != (row,col)) :::
		listOfBox(row, col).filter(_ != (row, col))
	}

}

// Top - left corner is (0 ,0). Bottom - right corner is (8 ,8). Feel free to
// change the fields of this class .
class Board ( val available : Map[(Int, Int) , List [Int]]) extends BoardLike[Board]{
	def availableValuesAt ( row : Int , col : Int) : List[Int] = {
		// Assumes that a missing value means all values are available . Feel
		// free to change this .
		available.getOrElse((row,col),1.to(9).toList)}
	def valueAt ( row : Int , col : Int ): Option [ Int ] = {
		if(available.apply((row,col)).size == 1) Some(available.apply(row,col)(0))
		else None}
	def isSolved (): Boolean = available.filter(x => available.apply(x._1).size != 1).size == 0 
	def isUnsolvable (): Boolean = available.filter(x => available.apply(x._1).size == 0).size != 0
	
	def place ( row : Int , col : Int , value : Int ): Board = {
		require (availableValuesAt(row,col).contains(value))	

		def placeHelper2(list : List[(Int, Int)], map : Map[(Int, Int) , List [Int]]) : List[(Int, Int)] = {
			list match {
				case Nil => Nil
				case h :: t => if(map.apply(h).size == 1) placeHelper2(t, map)
								else h :: placeHelper2(t, map)
			}}
		def placeHelper(list : List[(Int, Int)], value : Int, map : Map[(Int, Int) , List [Int]], k : Int) : Map[(Int, Int) , List [Int]] = {
			if(k < list.size){
				val temp = map.updated(list(k), map.apply(list(k)).filter(_ != value))
				val tempList = map.apply(list(k)).filter(_ != value)
				if(tempList.size == 1){
					val temp2 = placeHelper(placeHelper2(Solution.peers(list(k)._1, list(k)._2),temp), tempList(0), temp, 0)
					placeHelper(list, value, temp2, k+1)
				} else{
					placeHelper(list, value, temp, k +1)
				}
			}else {
				map
			}}

		new Board(placeHelper(Solution.peers(row,col), value, available, 0).updated((row,col), List(value)))

	}

	// You can return any Iterable (e.g., Stream )
	def nextStates (): List [ Board ] = {

		def nextStatesHelper2(list : List[Int], x : (Int, Int), board : Board) : List[Board] = {
			list match{
				case Nil => Nil
				case h :: t => board.place(x._1, x._2, h) :: nextStatesHelper2(t, x, this)
			}}
		def nextStatesHelper(map : Map[(Int, Int) , List [Int]], list : List[(Int, Int)]) : List[Board] = {
			list match {
				case Nil => Nil
				case h :: t => if(map.apply(h).size == 1) nextStatesHelper(map, t)
								else nextStatesHelper2(map.apply(h), h, this) :::  nextStatesHelper(map, t)
			}}
		def updateListofBoard2(board : Board, list : List[(Int, Int)]) :  List[Int] ={
			list match {
				case Nil => Nil
				case h :: t => board.availableValuesAt(h._1, h._2) ::: updateListofBoard2(board, t)
			}}

		if ( isUnsolvable ()) {
			List ()
		}
		else {
			val list = available.keysIterator.toList
			nextStatesHelper(available, list).sortWith(updateListofBoard2(_, list).size <  updateListofBoard2(_, list).size).filter(x => x.isUnsolvable == false)
		}
	}

	def solveHelper(list : List[Board]) : Option[Board] = {
		list match {
			case Nil => None
			case h :: t => if(h.isSolved) Some(h)
							else {
								if(h.solve == None){
									solveHelper(t)
								} 
								else h.solve
							}
		}
	}

	def solve (): Option [ Board ] = {
		if(isSolved) Some(this)
		else{
			solveHelper(nextStates)
		}
	}
}
