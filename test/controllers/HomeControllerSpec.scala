package controllers

import org.scalatestplus.play._
import org.scalatestplus.play.guice._
import play.api.test._
import play.api.test.Helpers._

class HomeControllerSpec extends PlaySpec with GuiceOneAppPerTest with Injecting {
  
  " ------ Tests for Execise No. 1 ------" should {
    
    "valid for a Empty list" in {
      val controller = inject[HomeController]
      val resul = controller.flattenArrayFunction(List())
      
      resul mustBe List()
    }
    
    "valid for Lists with just Nil Values" in {
      val controller = inject[HomeController]
      val resul = controller.flattenArrayFunction(List(Nil, Nil, Nil))
      
      resul mustBe List()
    }
    
    "valid for Lists with just null Values" in {
      val controller = inject[HomeController]
      val resul = controller.flattenArrayFunction(List(null, null, null, List(null, null)))
      
      resul mustBe List()
    }
    
    "valid for Lists with Nil Values and numeric values" in {
      val controller = inject[HomeController]
      val resul = controller.flattenArrayFunction(List(1, List(2,3,Nil,4),List(Nil), 5))
      
      resul mustBe List(1,2,3,4,5)
    }
    
    "valid for Lists with null Values and numeric values" in {
      val controller = inject[HomeController]
      val resul = controller.flattenArrayFunction(List(1, List(2,3,null,4),List(null), 5))
      
      resul mustBe List(1,2,3,4,5)
    }
    
    "valid for Lists with null, Nil and numeric Values" in {
      val controller = inject[HomeController]
      val resul = controller.flattenArrayFunction(List(1, List(2,3,null,4),List(null), 5, List(6,Nil, List(null, 7, List(Nil, 8)))))
      
      resul mustBe List(1,2,3,4,5,6,7,8)
    }
    
    "valid for Lists with numbers and a sub list" in {
      val controller = inject[HomeController]
      val resul = controller.flattenArrayFunction(List(1,2,3,4,5,6,7,List(8,9),10))
      
      resul mustBe List(1,2,3,4,5,6,7,8,9,10)
    }
    
    "valid for Lists with only sub lists" in {
      val controller = inject[HomeController]
      val resul = controller.flattenArrayFunction(List(List(8,9),List(10,11),List(12,13)))
      
      resul mustBe List(8,9,10,11,12,13)
    }
    
    "valid for Lists with multiple sub lists and numbers" in {
      val controller = inject[HomeController]
      val resul = controller.flattenArrayFunction(List(List(1, 1), 2, List(3, List(5, 8))))
      
      resul mustBe List(1,1,2,3,5,8)
    }
    
    "valid for Lists with multiple sub lists, numbers and strings" in {
      val controller = inject[HomeController]
      val resul = controller.flattenArrayFunction(List(List(1, "queso"), 2, List(3.8, List(5, "jamon"))))
      
      resul mustBe List(1,"queso",2,3.8,5,"jamon")
    }
  }
  
  " ------ Tests for Execise No. 2 ------" should {
    
    "valid if a word starts with a vowel" in {
      val controller = inject[HomeController]
      val resul = controller.pigLatinFunction("apple")
      
      resul mustBe "appleay"
    }
    
    "valid if a word starts with a single consonant" in {
      val controller = inject[HomeController]
      val resul = controller.pigLatinFunction("beach")
      
      resul mustBe "eachbay"
    }
    
    "valid if a word starts with a two consonants" in {
      val controller = inject[HomeController]
      val resul = controller.pigLatinFunction("chair")
      
      resul mustBe "airchay"
    }
    
    "valid if a word starts with a three consonants" in {
      val controller = inject[HomeController]
      val resul = controller.pigLatinFunction("through")
      
      resul mustBe "oughthray"
    }
    
    "valid for several words - 1" in {
      val controller = inject[HomeController]
      val resul = controller.pigLatinFunction("hello world")
      
      resul mustBe "ellohay orldway"
    }
    
    "valid for several words - 2" in {
      val controller = inject[HomeController]
      val resul = controller.pigLatinFunction("Here are some questions to help you")
      
      resul mustBe "erehay areay omesay uestionsqay otay elphay ouyay"
    }
  }
  
  " ------ Tests for Execise No. 3 ------" should {
    
    "valid If it is None when there isn't any posible solution" in {
      val controller = inject[HomeController]
      val resul = controller.alphameticsFunction("AA + BB == CDEFG")
      
      resul mustBe None
    }
    
    "valid - puzzle with three letters" in {
      val controller = inject[HomeController]
      val resul = controller.alphameticsFunction("I + BB == ILL")
      
      resul mustBe Some(Map('B' -> 9, 'I' -> 1, 'L' -> 0))
    }
    
    "valid - puzzle with eight letters" in {
      val controller = inject[HomeController]
      val resul = controller.alphameticsFunction("SEND + MORE == MONEY")
      
      resul mustBe Some(Map('D' -> 7,'E' -> 5,'M' -> 1,'N' -> 6,'O' -> 0,'R' -> 8,'S' -> 9,'Y' -> 2))
    }
    
    "valid - puzzle with ten letters" in {
      val controller = inject[HomeController]
      val resul = controller.alphameticsFunction("FIFTY + STATES == AMERICA")
      
      resul mustBe Some(Map('A' -> 1, 'C' -> 3, 'E' -> 4, 'F' -> 6, 'I' -> 5, 'M' -> 0, 'R' -> 7, 'S' -> 9, 'T' -> 8, 'Y' -> 2))
    }
  }
}
