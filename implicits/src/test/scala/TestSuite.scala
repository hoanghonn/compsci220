import DateImplicits._
import PathImplicits._
import java.nio.file.Paths
import java.nio.file.Path
import java.nio.file.Files
import java.nio.file.StandardOpenOption
import java.time.LocalDate

class TestSuite extends org.scalatest.FunSuite { 
	val path = Paths.get("a", "b")
	val path2 = Paths.get("c", "d")
	//create a test folder inside implicits folder
	val path3 = Paths.get("test","test.txt")

	test("test / 1") {
		assert("a"/("b") == "a"/"b")
	}

	test("test / 2") {
		assert(path/("c") == "a"/"b"/"c")
	}

	test("test / 3") {
		assert(path/(path2) == "a"/"b"/"c"/"d")
	}

	test("test write") {
		assert(path3.write("hello \n") == path3)
	} 

	test("test read"){
		assert(path3.read == "hello \n")
	}

	test("test append"){
		assert(path3.append("it's me").read == "hello \nit's me")
	}

	test("test date1"){
		assert(2.jan == LocalDate.of(2016, 1, 2))
	}

	test("test date2"){
		assert(2.jan(2012) == LocalDate.of(2012, 1, 2))
	}

	test("test date3"){
		assert(29.feb(2016) == LocalDate.of(2016, 2, 29))
	}

	test("test date4"){
		assert(29.feb(2016) +(2.days) == LocalDate.of(2016, 3, 2))
	}

}