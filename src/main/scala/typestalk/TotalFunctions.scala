package typestalk

object TotalFunctions {

  /**
   * Cast your mind back to high school maths. Remember what 'function' meant in school? 
   * It had approximately the following type and you plotted it on the x-y axes.
   */
  val fSchool: (Double) => (Double) = ???

  /**
   * Hopefully you recall studying the /domain/ (acceptable inputs) and /range/ (possible outputs) 
   * of a function.
   *
   * And perhaps you recall functions like...
   */
  val sqrt: Double => Double = ??? // √x
  
  val reciprocal: Double => Double = ??? // 1/x

  val probabilityComplement: Double => Double = ??? // 1 - P

  
  /* ..but what about:  */
  val moreComplexThanScala = sqrt(-1.0)
  
  val insideTheEventHorizon = reciprocal(0.0)
  
  val `!!*>Extremely<*!!_Unlikely` = probabilityComplement(7.0)

  
  /**
   * I hope you can see that the type signature 'Double => Double' above is a little 'white' lie? 
   * Can we improve our lying ways?
   *
   * A /total function/ is defined over its whole domain, whereas a /partial function/ is defined 
   * over a subset of its domain. In a statically typed world, if we have a function...
   */

  def f[A, B]: (A) => B = ???

  /**
   * .. if its a total function then we're stating that every value of type A is acceptable input.
   *
   * We static-types like to think we're the careful, responsible ones (and we're probably right), 
   * but we flout this principle incessantly:
   */

  
  /** @param jobCode Consists of <BusinessUnitID (3 letters)>-<ChargeCode (8 digits)>-<task number>, 
   * eg "FIN-0064308-987" */
  def findTask(jobCode: String) = { ??? }

  findTask("IamTheWalrus")

  
  
  /**
   * Steps towards making your functions a less partial:
   *
   * 1. Awareness. Each time you write function, ask yourself
   * "How much of the domain type do I really accept? How tightly does the range type describe the output?" Aim to improve both.
   *
   * 2. Action. Invent precise types that describe your data. Validate unknown data and 
   * tag it with precise types if it passes validation.
   * Don't think that because your value is a subset of a built-in type, for example expressible 
   * by an Int or String, that that's good enough.
   * 
   * Use static typing like you believe in it, goddammit!
   */

  
  /** Lets use Probability as a case study of how to do this in scala. 
   * We can model Probability as a floating point number, always in [0, 1]. */


  /** This is a "value class" because it extends AnyVal
   * Modifier protected[typestalk] means its constructor is only callable from "typestalk" (ie this) package. 
   * 
   * (Value classes can only be declared in a "static" context: top-level or in an object)
   * */
  class Probability protected[typestalk] (val value: Double) extends AnyVal {
    
     /* Multiplying probabilities is common, and fortunately inherently safe.
     * A win for static typing.
     * */
    def *(b: Probability) = new Probability(value * b.value)
  
    /** But addition, subtraction, and division can still fail at runtime.
     * 
     * By re-validating each operation, we ensure errors in our program logic "fail fast",
     * but it's not a convincing win IMO.
     * */
    def +(b: Probability) = asP(value + b.value)
    def -(b: Probability) = asP(value - b.value)
    def /(b: Probability) = asP(value / b.value)
  
  
    /** One more operation that's statically guaranteed safe*/
    def or(b: Probability) = this + b - this * b
  }


  val ProbabilityRange = com.google.common.collect.Ranges.closed[java.lang.Double](0, 1)

  /**
   * Lets look at 2 ways to go from a Double to a Probability. If we know it won't fail, or don't care if it does, we could simply coerce
   * a Double into a probability and blowup with an Exception otherwise...
   */

  def asP(p: Double): Probability = {
    validateP(p).getOrElse(throw new IllegalArgumentException(p.toString))
  }

  /**
   * The above is OK for a small script. But in a library or proper application, its more graceful 
   * to validate the Double is a Probability.
   * Think of a validation as a sort into one of two buckets, which can have different types. 
   * The right bucket is the success type, the left bucket is the fail type.
   *
   * Note that our fail type is Double, not a String error message. I chose this because a fail message 
   * can be trivially derived from the Double value, but its harder to recover the erroneous Double 
   * value once its wrapped into a human readable string.
   * (i.e. String has higher entropy than Double)
   */
  import scalaz._
  import scalaz.Scalaz._

  def validateP(p: Double): Validation[Double, Probability] = {
    if (ProbabilityRange.contains(p)) {
      new Probability(p).success
    } else {
      p.fail
    }
  }

  /**
   * I like to think of the validation step as forming a barrier around a richly-typed domain model, 
   * like the membrane around a cell. 
   * 
   * When we pass from the weakly typed outside into the domain, we must get through the validation, 
   * but inside we can count on the invariants enforced by richer type system holding true. 
   * 
   * The larger/more complex the domain, the more benefit we gain from having
   * this membrane since we pay the validation cost only once at the boundary.
   *
   * While this is a type of "defensive-programming", it is distinctly different from the 
   * "trust no-one" style one often sees in large systems, where every piece or function distrusts
   * its inputs and re-validates it. 
   * We seek efficiency and safety in the domain, by using the type system and validation 
   * to concentrate trust checks out at the system boundaries.
   */

  

  /**
   * How to treat Probability as a Numeric type? 
   * eg to enable summing over a collection of them...
   * 
   * This demonstrate why splitting Numeric into finer grained pieces is needed,
   * (as I advocate here [https://issues.scala-lang.org/browse/SI-5202]).
   * 
   * Currently, Numeric is like a restaurant which only offers a 10 course banquet.
   * Oh, well.. lets feast then!
   */

  implicit val ProbabilityFractional = new Fractional[Probability] {
    
    //make sense for Probability
    def plus(x: Probability, y: Probability): Probability = x + y
    def minus(x: Probability, y: Probability): Probability = x - y
    def times(x: Probability, y: Probability): Probability = x * y
    def div(x: Probability, y: Probability): Probability = x / y
    def toDouble(x: Probability): Double = x.value
    def toFloat(x: Probability): Float = x.value.toFloat

    //dont make sense
    def negate(x: Probability): Probability = ???
    def fromInt(x: Int): Probability = ???
    def toInt(x: Probability): Int = ???
    def toLong(x: Probability): Long = ???
  }

  /**
   * Next topic: Good Types and Bad Types....
   * 
   * We have a type for Integers. 
   * 
   * But what about a type for Integers between 1 and 6? To represent a die roll perhaps.
   */
  class Nat6(val value: Int) extends AnyVal {
    def +(other: Nat6) = value + other.value
    def -(other: Nat6) = value + other.value
    def *(other: Nat6) = value + other.value
    def /(other: Nat6) = value / other.value
  }
  
  def validateNat6(value: Int): Validation[Int, Nat6] = {
    if ((1 to 6).contains(value)) { new Nat6(value).success} 
    else {value.fail}
  }
  
  /** The problem with this type is that most operation we can define over of it "pop out" of the
   * narrow type into the more general Int type. 
   * 
   * In algebra, I think we'd say that Nat6 is not "closed" under most useful operations. 
   * 
   * Ideally Types are closed under useful operations. In the language of category theory, we'd say that
   * they have many useful endofunctions.
   * 
   * For example, Strings can be sliced, concatenated, reversed and spliced and still remain a String.
   * Similarly for Ints.
   * 
   * The Probability use case lies somewhere in between. Its closed under multiplication (perhaps
   * it's most crucial operation) but open under addition, division and subtraction.
   * 
   * Of course, Any is closed under all operations. Is it the best type therefore? 
   * That way leads to untyped languages....
   * 
   * So there's a design tradeoff between lax types, which have good closure, and strict types
   * which accurately describe values, but have poor closure. 
   * */
  
  /** 
   * Types that describe Aggregate Properties
   * 
   * A ProbabilityDistribution assigns a Probability to all events in the space. 
   * But further, the sum of all possible event-probabilities is always 1.0
   */
  
  trait EventSpace {
    type Event
  }
  trait ProbabilityDistribution {
    
    val eventSpace: EventSpace
    
    /* Note use of a "path dependent type" here; type 'Event' is reached via a term 'eventSpace'. 
     * To work, eventSpace must be a 'stable identifier', eg declared a val not a def.  */
    
    def p(event: eventSpace.Event): Probability
  }
  
   /* What novel about this type is that is expresses a constraint across an /aggregate collections of values/.
   * ie that they all sum to 1.
   * 
   * What are some other examples of describing aggregate qualities of a collection via types?
   *  
   * Ordering of elements?
   * 
   *   SortedSet, SortedMap 
   * 
   * Distinctness of elements?
   * 
   *   Set
   *   
   * The norm of the elements? 
   * 
   * In an earlier talk on typing that I gave back in February, I show how the 'with' keyword
   * can attach type tags to enrich a simpler type with extra meaning.
   * 
   * For example, to signal that a direction Vector has unit length (ie magnitude of 1.0)*/
  type Bar = {def direction: Foo}
  type Foo = Bar with UnitLength
  class UnitLength
  case class Vector2[T: Numeric](x: T, y: T) extends UnitLength {
    def direction: Foo = ???
  }
  
  

  
  /** Wrap up with question to the audience: can anyone think of any other types that 
   * describe aggregate qualities of a collection of things, rather than the things themselves?*/
  
  
  
  /** Lets look at some of the other examples I gave at the start of the talk.*/

  class NonNegative(val value: Double) extends AnyVal {
    
    def sqrt: NonNegative = new NonNegative(Math.sqrt(value))
    
    def +(other: NonNegative) = new NonNegative(value + other.value)
  }
  def validateNonNegative(n: Double): Validation[Double, NonNegative] = ???

  
  //skipping over the definitions of these
  class BusinessUnit(val unitId: String) extends AnyVal
  class ChargeCode(val code: String) extends AnyVal
  class Nat(val value: Int) extends AnyVal
  class Task

  def validateBusinessUnit(unitId: String): ValidationNEL[String, BusinessUnit] = ???
  def validateChargeCode(code: String): ValidationNEL[String, ChargeCode] = ???
  def validateNatural(taskNum: Int): ValidationNEL[String, Nat] = ???
  
  def findTask(unit: BusinessUnit, code: ChargeCode, taskNum: Nat): Task = ???
  
  /** Here we can see an example of some extremely useful library code from Scalaz, the 
   * ApplicativeBuilder, in action. We want to glue together three smaller validations 
   * into a larger one, returning either
   * - a valid Task in the success case
   * - one or more reasons why validation failed
   * 
   * To compose the larger validation, we must specify:
   * - The smaller input validations
   * - How to compose the inputs into the out, in the event they are all valid
   * 
   *  The rest of the boilerplate is factored away into Scalaz.*/
  
  def validateTask(unitId: String, code: String, taskNum: Int): ValidationNEL[String, Task] = {
    (validateBusinessUnit(unitId) |@| 
        validateChargeCode(code) |@| 
        validateNatural(taskNum)) 
          {(unit, code, num) => findTask(unit, code, num) } 
  }

  /**
   * What about 'reciprocal: Double => Double', ie 1/x ? In this case, the function is defined everywhere except for a single hole
   * or singularity at x == 0. Will it really widely be useful to have
   */
  trait NonZero
  def validateNonZero(n: Double): Validation[Double, NonZero] = ???
  def reciprocal(n: NonZero): NonZero = ???

  /**
   * It depends on your problem domain; sometime yes, but sometimes not.
   *
   * When I was preparing this presentation, I initially thought "Reciprocal not a good clear example,
   * I should remove it". But I decide to leave it in, because unfortunately, the real world is not black and white
   * but rather shades of grey, and judgment will always be required in deciding whether specific types are warranted.
   */

}