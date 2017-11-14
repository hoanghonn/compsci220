object PathImplicits {
	import java.nio.file.Paths
	import java.nio.file.Path
	import java.nio.file.Files
	import java.nio.file.StandardOpenOption


	implicit class RichPathImplicits(str : String) {
		def /(str2 : String) = Paths.get(str,str2)	

	}

	implicit class RichPath2(path : Path) {
		def / (str : String) = Paths.get(path.toString, str)

		def / (path2 : Path) = Paths.get(path.toString, path2.toString)
	
		def write(str : String) =  Files.write(path, str.getBytes)
			
		def read() : String = {
			val bytes = Files.readAllBytes(path)
			new String(bytes)
		}

		def append(str : String) = {
			if(Files.exists(path) == false){
				path.write(str)
			} else{
				Files.write(path, str.getBytes, StandardOpenOption.APPEND)
			}
		}
	}

}

object DateImplicits {
	import java.time.LocalDate
	
	implicit class RichLocalDate(val day : Int) {
		def jan() = LocalDate.of(LocalDate.now.getYear, 1, day)
		def feb() = LocalDate.of(LocalDate.now.getYear, 2, day)
		def mar() = LocalDate.of(LocalDate.now.getYear, 3, day)
		def apr() = LocalDate.of(LocalDate.now.getYear, 4, day)
		def may() = LocalDate.of(LocalDate.now.getYear, 5, day)
		def jun() = LocalDate.of(LocalDate.now.getYear, 6, day)
		def jul() = LocalDate.of(LocalDate.now.getYear, 7, day)
		def aug() = LocalDate.of(LocalDate.now.getYear, 8, day)
		def sep() = LocalDate.of(LocalDate.now.getYear, 9, day)
		def oct() = LocalDate.of(LocalDate.now.getYear, 10, day)
		def nov() = LocalDate.of(LocalDate.now.getYear, 11, day)
		def dec() = LocalDate.of(LocalDate.now.getYear, 12, day)

		def jan(year : Int) = LocalDate.of(year, 1, day)
		def feb(year : Int) = LocalDate.of(year, 2, day)
		def mar(year : Int) = LocalDate.of(year, 3, day)
		def apr(year : Int) = LocalDate.of(year, 4, day)
		def may(year : Int) = LocalDate.of(year, 5, day)
		def jun(year : Int) = LocalDate.of(year, 6, day)
		def jul(year : Int) = LocalDate.of(year, 7, day)
		def aug(year : Int) = LocalDate.of(year, 8, day)
		def sep(year : Int) = LocalDate.of(year, 9, day)
		def oct(year : Int) = LocalDate.of(year, 10, day)
		def nov(year : Int) = LocalDate.of(year, 11, day)
		def dec(year : Int) = LocalDate.of(year, 12, day)
	}

	implicit class RichLocalDate2(val ld : LocalDate){
		def +(x : (Int,Int)) = {
			if(x._2 == 1) ld.plusDays(x._1)
			else if(x._2 == 2) ld.plusMonths(x._1)
			else ld.plusYears(x._1)
		}
	}

	implicit class RichNum(val num : Int){
		def days() : (Int,Int) = (num, 1)
		def months() : (Int, Int) = (num, 2)
		def years() : (Int, Int) = (num, 3)
	}
}