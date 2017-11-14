import Regexes._
import scala.util.matching.Regex

class TrivialTestSuite extends org.scalatest.FunSuite {
	val x1 = "*!(#!"
	val x2 = "An|-|CuongDepTraiVC()@''[p"
	val b1 = "00:00"
	val b2 = "23:42"
	val b3 = "23:00"
	val c1 = "(206) 313-1808"
	val c2 = "(000) 125-6894"	
	val d1 = "02149"
	val d2 = "02149-1314"
	val e = "/* sadsada \n 21321@DSA */"
	val f = "forty-six"
	val g = "XXXVIII"
	val h1 = "2015-02-29"
	val h2 = "2017-03-31"
	val h3 = "2000-02-28"

	test (" The Regexes object must be defined ") {
		val regexes : hw.regex.RegexLike = Regexes
	}
	test("a1"){
		assert(notAlphanumeric.pattern.matcher(x1).matches)
	}
	test("a2"){
		assert(notAlphanumeric.pattern.matcher(x2).matches == false)
	}

	
	test("b1"){
		assert(time.pattern.matcher(b1).matches)
	}
	test("b2"){
		assert(time.pattern.matcher(b2).matches)
	}
	test("b3"){
		assert(time.pattern.matcher(b3).matches)
	}


	test("c1"){
		assert(phone.pattern.matcher(c1).matches)
	}
	test("c2"){
		assert(phone.pattern.matcher(c2).matches)
	}
	

	test("d1"){
		assert(zip.pattern.matcher(d1).matches)
	}
	test("d2"){
		assert(zip.pattern.matcher(d2).matches)
	}


	test("e"){
		assert(comment.pattern.matcher(e).matches)
	}


	test("f"){
		assert(numberPhrase.pattern.matcher(f).matches)
	}


	test("g"){
		assert(roman.pattern.matcher(g).matches)
	}

	
	test("h1"){
		assert(date.pattern.matcher(h1).matches == false)
	}
	test("h2"){
		assert(date.pattern.matcher(h2).matches)
	}
	test("h3"){
		assert(date.pattern.matcher(h3).matches)
	}

	
	test("z"){
		assert(evenParity.pattern.matcher("2").matches)
	}

	
}