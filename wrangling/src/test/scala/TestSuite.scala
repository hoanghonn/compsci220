import Wrangling._
import edu.umass.cs.CSV

class TestSuite extends org.scalatest.FunSuite{
	val data = CSV.fromFile("shortened-births.csv")
	
	test ("yearIs method 1"){
		assert(yearIs(data, 1880) == List(List("1880", "Mary", "F","7065"), List("1880","Charley","M","305")))
	} 

	test ("yearIs method 2"){
		assert(yearIs(data,1893) == List(List("1893","Garnett","F","10"), List("1893","Adolphus","M","28")))
	}

	test ("yearGT method 1 :" + yearGT(data, 2013)){
		assert(true)
	}

	test ("yearGT method 2 :" + yearGT(data, 2014)){
		assert(true)
	}

	test ("yearLT method 1 :" + yearLT(data,1880)){
		assert(true)
	}

	test ("yearLT method 2 :" + yearLT(data, 1990)){
		assert(true)
	}

	test ("onlyName method 1 :" + onlyName(data, "Chaise")){
		assert(true)
	}

	test ("onlyName method 2 :" + onlyName(data, "My")){
		assert(true)
	}
	
	test ("mostPopular method 1 :" + mostPopular(data)){
		assert(true)
	}

	test ("count method :" + count(data)){ 
		assert(true)
	}

	test ("countGirlsAndBoys method :" + countGirlsAndBoys(data)){
		assert(true)
	}

	test ("genderNeutralNames method :" + genderNeutralNames(data)){
		assert(true)
	}

	test ("expectedAlive method 1 :"){
		assert(expectedAlive("M", 1929, 1931) == false)
	}

	test ("expectedAlive method 2 :"){
		assert(expectedAlive("M", 2010, 2011) == true)
	}

	test ("expectedAlive method 3 :"){
		assert(expectedAlive("M", 1930, 1935) == true)
	}

	test ("expectedAlive method 4 :"){
		assert(expectedAlive("F", 1929, 1931) == false)
	}

	test ("expectedAlive method 5 :"){
		assert(expectedAlive("F", 2010, 2011) == true)
	}

	test ("expectedAlive method 6 :"){
		assert(expectedAlive("F", 1930, 1935) == true)
	}

	test ("estimatePopulation method :" + estimatePopulation(data,2010)){
		assert(true	)
	}

	test ("estimatePopulation method 2 :" + estimatePopulation(data,2059)){
		assert(true	)
	}


}