import hw.parsing._
import scala.util.parsing.combinator. _

object ArithEval extends ArithEvalLike {
	def eval ( e: Expr ): Double = {
		e match{
			case Num(n) => n
			case Add(e1, e2) => eval(e1) + eval(e1)
			case Sub(e1, e2) => eval(e1) - eval(e2)
			case Mul(e1, e2) => eval(e1) * eval(e2)
			case Div(e1, e2) => eval(e1) / eval(e2)
			case Exponent(e1, e2) => Math.pow(eval(e1), eval(e2))
		}
	}
}
object ArithParser extends ArithParserLike {
	// number : PackratParser [ Double ] is defined in ArithParserLike
	lazy val atom : PackratParser [ Expr ] = number ^^ { case n => Num(n)} | "(" ~>expr<~ ")"
	lazy val exponent : PackratParser [ Expr ] = exponent ~ "^" ~ atom ^^ { case e ~ _ ~ a => Exponent(e,a)} | atom 
	lazy val mul : PackratParser [ Expr ] = mul ~ "*" ~ exponent ^^ { case m ~ _ ~ e => Mul(m,e)} | mul ~ "/" ~ exponent ^^ { case m ~ _ ~ e => Div(m,e)} | exponent
	lazy val add : PackratParser [ Expr ] = add ~ "+" ~ mul ^^ { case a ~ _ ~ m => Add(a,m)} | add ~ "-" ~ mul ^^ { case a ~ _ ~ m => Sub(a,m)} | mul
	lazy val expr : PackratParser [ Expr ] = add
}
object ArithPrinter extends ArithPrinterLike {
	def print (e: Expr ): String = {
		e match{
			case Num(n) => n.toString
			case Add(a,b) => print(a).toString + "+" + print(b).toString
			case Sub(a,b) => print(a).toString + "-" + print(b).toString
			case Mul(a,b) => print(a).toString + "*" + print(b).toString
			case Div(a,b) => print(a).toString + "/" + print(b).toString
			case Exponent(a,b) => print(a).toString + "*" + print(b).toString
		}
	}
}