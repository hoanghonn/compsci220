import scala.util.matching.Regex 	

object Regexes extends hw.regex.RegexLike {
	def notAlphanumeric : Regex = "[^A-Za-z0-9 ]*".r
	def time : Regex = "(\\d|0\\d|1\\d|2[0-3]):[0-5]\\d".r
	def phone : Regex = "\\(\\d{3}\\)\\s\\d{3}-\\d{4}".r
	def zip : Regex = "\\d{5}|\\d{5}-\\d{4}".r
	def comment : Regex = "\\/\\*(.|\\n)*\\*\\/".r
	def numberPhrase : Regex = "\\btwenty-\\b(one|two|three|four|five|six|seven|eight|nine)|\\bthirty-\\b(one|two|three|four|five|six|seven|eight|nine)|\\bforty-\\b(one|two|three|four|five|six|seven|eight|nine)|\\bfifty-\\b(one|two|three|four|five|six|seven|eight|nine)|\\bsixty-\\b(one|two|three|four|five|six|seven|eight|nine)|\\bseventy-\\b(one|two|three|four|five|six|seven|eight|nine)|\\beighty-\\b(one|two|three|four|five|six|seven|eight|nine)|\\bninety-\\b(one|two|three|four|five|six|seven|eight|nine)".r
	def roman : Regex = "(XL|X{0,3})(IX|IV|V?I{0,3})".r
	def date : Regex = "((((19|20)(([02468][048])|([13579][26]))-02-29))|(\\d{4})-((((0[1-9])|(1[0-2]))-((0[1-9])|(1[0-9])|(2[0-8])))|((((0[13578])|(1[02]))-31)|(((0[1,3-9])|(1[0-2]))-(29|30)))))".r
	def evenParity : Regex = "(([02468]*[13579]){2})*[02468]*|(([02468]*[13579]){2})".r
}
