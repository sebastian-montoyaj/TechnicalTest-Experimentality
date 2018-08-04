package controllers

import javax.inject._
import play.api._
import play.api.mvc._

import scala.util.parsing.combinator.RegexParsers
import Alphametics.Solution

@Singleton
class HomeController @Inject()(cc: ControllerComponents) extends AbstractController(cc)
{
  // Constants -------------------------------------
  val vowels = Array('a','e','i','o','u')
  
  // Exercise No. 1 --------------------------------
  // Recursive function which it'll flatten any nested list
  // A is a type class in order to ease the casting and fusion between lists. More info: http://blog.leifbattermann.de/2017/03/25/what-are-scala-type-classes/
  // Parameters:    lista is the nested list to simpplify
  // Return:        Simplified list
  def flattenArrayFunction[A](lista: List[Any]): List[A] = {
    var acumulator = List[A]() // Empty list where i'm going to add all elements in the nested list
    
    // Now, for each element in the nested list do
    lista foreach (
      // if the current element is
      element => element match {
      // A list then concatenate the accumulated elements list with the result from flattened this sublist
      case subLista: List[Any] => acumulator = acumulator ::: flattenArrayFunction(subLista)
      // A value then add this simple element to the accumulated elements list
      case value: A => acumulator = acumulator :+ value
      // A null value then simply return the accumulated list
      case null => acumulator
    })
    
    // Finally, I return  the accumulated elements list
    acumulator
  }
  
  // Exercise No. 2 --------------------------------
  // Function to translate english to pig latin
  // Parameters:    English String to translate
  // Return:        Pig Latin String
  def pigLatinFunction(text2translate: String) = {
    // First, I split the string in words (I assume that every word is separated by a space)
    val splittedWords = text2translate.toLowerCase().split(" ")
    
    // Second, I create a var in order to store the translations
    var translatedWords = List[String]()
    
    // Now, every word is going to translated acording to...
    for (word <- splittedWords)
    {
      // If the word starts with a vowel then
      if (vowels.contains(word(0)))
      {
        // I add "ay" at the end of the word and store it
        val auxPigLatinWord1 = word + "ay"
        translatedWords = translatedWords :+ auxPigLatinWord1
      }
      else // If it starts with a consonant then
      {
        // I search for the position of the first vowel
        val indexVowel = word.indexWhere(c => vowels.contains(c))
        
        // and put the consonants at the end of the word with "ay", also I store the word
        val auxPigLatinWord2 = word.drop(indexVowel) + word.take(indexVowel) + "ay"
        translatedWords = translatedWords :+ auxPigLatinWord2 
      }
    }
    
    // Finally, I return the list of translated words as a String
    translatedWords.mkString(" ")
  }
  
  // Exercise No. 3 --------------------------------
  // Funtion to solve alphametics puzzle
  def alphameticsFunction(puzzle2solve: String) = {
    Alphametics.solve(puzzle2solve)
  }
  
  def index() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.index())
  }
}


// ------------------------------------------------------------------------------------------------------------------------------
// The below code was extracted from: https://github.com/exercism/scala/tree/master/exercises/alphametics
// All rights belong to their respective owners

object Alphametics {
  type Puzzle = String
  type Solution = Map[Char, Int]

  def solve(puzzle: Puzzle): Option[Solution] = {
    val expression = parse(puzzle)
    val uniqueLetters = puzzle filter (_.isLetter) distinct

    expression flatMap solveExpression(uniqueLetters)
  }

  private def parse(str: String): Option[Expression[Boolean]] =
    ExpressionParser.parse(str)

  private def solveExpression(uniqueLetters: String)(boolExpr: Expression[Boolean]): Option[Solution] = {
    def isSolution(solution: Solution): Boolean =
      boolExpr.eval(solution) getOrElse false

    def toSolution(tenChars: String): Solution =
      tenChars.zipWithIndex.toMap filterKeys (_.isLetter)

    val tenChars = uniqueLetters ++ Seq.fill(10 - uniqueLetters.size)('.')
    tenChars.permutations map toSolution find isSolution
  }
}


object ExpressionParser extends RegexParsers {

  private implicit def parseResultToOption[T](parseResult: ParseResult[T]): Option[T] =
    parseResult map (Some(_)) getOrElse None

  def parse(str: String): Option[Expression[Boolean]] =
    parseAll(booleanExpression, str)

  private def booleanExpression: Parser[Expression[Boolean]] =
    longExpression ~ "==" ~ longExpression ^^ {
      case left ~ _ ~ right => Equals(left, right)
    }

  private def longExpression: Parser[Expression[Long]] =
    longOperation | nonOperator

  private def longOperation: Parser[Expression[Long]] =
    nonOperator ~ operator ~ longExpression ^^ {
      case left ~ op ~ right => op(left, right)
    }

  private def operator: Parser[(Expression[Long], Expression[Long]) => Expression[Long]] =
    plus | mult | power

  private def nonOperator: Parser[Expression[Long]] =
    word | number

  private val word: Parser[Word] =
    "[A-Z]+".r ^^ (Word(_))

  private val number: Parser[Number] =
    "[0-9]+".r ^^ (n =>(Number(n.toLong)))

  private val plus: Parser[(Expression[Long], Expression[Long]) => Expression[Long]] =
    "+"  ^^ const(Plus(_, _))

  private val mult: Parser[(Expression[Long], Expression[Long]) => Expression[Long]] =
    "*" ^^ const(Mult(_, _))

  private val power: Parser[(Expression[Long], Expression[Long]) => Expression[Long]] =
    "^" ^^ const(Power(_, _))

  private def const[A](a: A)(ignore: Any) = a
}


sealed trait Expression[T]
{
  def eval(solution: Solution): Option[T]
}

case class Word(word: String) extends Expression[Long]
{
  override def eval(solution: Solution) =
    if (solution(word(0)) == 0) None
    else Some((word map solution mkString) toLong)
}

case class Number(number: Long) extends Expression[Long]
{
  override def eval(solution: Solution) = Some(number)
}

trait Operation[A,B] extends Expression[B]
{
  val op: (A, A) => B
  val left: Expression[A]
  val right: Expression[A]
  override def eval(solution: Solution) =
    for {
      l <- left.eval(solution)
      r <- right.eval(solution)
    } yield op(l, r)
}

case class Equals[A](override val left: Expression[A], override val right: Expression[A]) extends Operation[A, Boolean]
{
  override val op: (A, A) => Boolean = _ == _
}

case class Plus(override val left: Expression[Long], override val right: Expression[Long]) extends Operation[Long, Long]
{
  override val op: (Long, Long) => Long = _ + _
}

case class Mult(override val left: Expression[Long], override val right: Expression[Long]) extends Operation[Long, Long]
{
  override val op: (Long, Long) => Long = _ * _
}

case class Power(override val left: Expression[Long], override val right: Expression[Long]) extends Operation[Long, Long]
{
  override val op: (Long, Long) => Long = math.pow(_, _).toLong
}

// ------------------------------------------------------------------------------------------------------------------------------
