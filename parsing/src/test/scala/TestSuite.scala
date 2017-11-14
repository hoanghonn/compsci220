import ArithEval._
import ArithParser._
import ArithPrinter._
import hw.parsing._

class TrivialTestSuite extends org . scalatest . FunSuite {
	test (" several objects must be defined ") {
		val parser : hw.parsing.ArithParserLike = ArithParser
		val printer : hw. parsing.ArithPrinterLike = ArithPrinter
		val eval : hw.parsing. ArithEvalLike = ArithEval
	}

	test("1)"){
		assert(parseArith("1 + -2") == Num(10))
	}
	test("2)"){
		assert(parseArith("10^3") == Num(1000))
	}
	test("3)"){
		assert(parseArith("2 * 3 + 5 * -10") == Num(10))
	}
	test("4)"){
		assert(parseArith("2 * (3 + 5) * -10") == Num(10))
	}
	test("5)"){
		assert(parseArith("2 * (3 + 5) ^ 2 * -10") == Num(10))
	}
	test("6)"){
		assert(print(parseArith("2 * (3 + 5) ^ 2 * -10")) == parseArith("2 * (3 + 5) ^ 2 * -10"))
	}

}