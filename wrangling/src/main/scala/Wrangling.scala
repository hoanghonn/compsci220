import  edu.umass.cs.CSV

object Wrangling {
	def yearIs(data : List[List[String]], n : Int): List[List[String]] = { 
		data.filter(x => x(0).toInt == n)
	}

	def yearGT(data: List[List[String]], bound : Int): List[List[String]] = { 
		data.filter(x => x(0).toInt > bound)
	}

	def yearLT(data: List[List[String]], bound : Int): List[List[String]] = {
		data.filter(x => x(0).toInt < bound)
	}

	def onlyName(data: List[List[String]], name: String) : List[List[String]] = { 
		data.filter(x => x(1) == name)
	}

	def mostPopularOp(data : List[List[String]]) : Int = {
		data match {
			case Nil => 0
			case h :: t => h(3).toInt + mostPopularOp(t)
		}
	}

	def mostPopular(data : List[List[String]]) : (String, Int) = {
		data.groupBy(x => x(1)).map(t => (t._1, mostPopularOp(t._2))).maxBy(_._2)
	}

	def count(data : List[List[String]]) : Int = { 
		data match { 
			case Nil => 0
			case h :: t => h(3).toInt + count(t)
		}
	}

	def femaleLst(data : List[List[String]]) : List[List[String]] = {
		data.filter(x => x(2) == "F")
	}

	def maleLst(data : List[List[String]]) : List[List[String]] = {
		data.filter(x => x(2) == "M")
	}

	def countGirlsAndBoys(data : List[List[String]]) : (Int,Int) = (
		count(maleLst(data)), count(femaleLst(data))
	)

	def maleNames(data : List[List[String]]) : Set[String] = {
		data.filter(x => x(2) == "M").map(x => x(1)).toSet
	}

	def femaleNames(data : List[List[String]]) : Set[String] = {
		data.filter(x => x(2) == "F").map(x => x(1)).toSet
	}

	def genderNeutralNames(data : List[List[String]]) : Set[String] = { 
		maleNames(data).intersect(femaleNames(data))
	}

	def expectedAlive(gender : String, birthYear : Int, currentYear : Int) : Boolean = {
		val data = CSV.fromFile("cdc-life-expectancy.csv")
		val data1 = yearIs(data,birthYear)
		if(gender == "M" && birthYear >= 1930 && birthYear <= 2010) { data1(0)(1).toInt + birthYear >= currentYear }
		else if(gender == "F" && birthYear >= 1930 && birthYear <= 2010) { data1(0)(2).toInt + birthYear >= currentYear }
		else { false }
	}

	def estimatePopulation(data : List[List[String]], year : Int) : Int = {
		data match { 
			case Nil => 0 
			case h :: t => h(2) match {
				case "M" => if (expectedAlive("M", h(0).toInt, year) == true) { h(3).toInt + estimatePopulation(t,year)}
							else estimatePopulation(t,year)
				case "F" => if (expectedAlive("F", h(0).toInt,year) == true) { h(3).toInt + estimatePopulation(t,year)}
							else estimatePopulation(t,year)
			}
						
		}
	}



}
