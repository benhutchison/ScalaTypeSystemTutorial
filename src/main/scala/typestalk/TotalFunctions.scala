package typestalk

class TotalFunctions {
  
/** Cast your mind back to high school maths. Remember what 'function' meant in Year 11? It had approximately the following type and you 
plotted it on the x-y axes.*/
  val fYr11: (Double) => (Double)

/** Hopefully you recall studying the /domain/ (acceptable inputs) and /range/ (possible outputs) of a function.

And perhaps you recall functions like...
*/
val sqrt: Double => Double  // √x

val reciprocal: Double => Double // 1/x

val probabilityComplement: Double => Double // 1 - P


val moreComplexThanScala = sqrt(-1.0)
val insideTheEventHorizon = reciprocal(0.0)
val `!!*>Extremely<*!!_Unlikely` =  probabilityComplement(7.0)
  
/**  I hope you can see that the type signature 'Double => Double' above is a little 'white' lie? Can we improve our lying ways? 

A /total function/ is defined over its whole domain, whereas a /partial function/ is defined over a subset of its domain.
In a statically typed world, if we have a function... */
  type A
  type B
  
  val f: (A) => B
  
/** .. if its a total function then we're stating that every value of type A is acceptable input.

We static-types like to think we're the careful, responsible ones (and we're probably right), but we flout this principle incessantly: */

/** @param jobCode Consists of <BusinessUnitID (3 letters)>-<ChargeCode (8 digits)>-<task number>, eg "FIN-0064308-987" */  
def findTask(jobCode: String) = {???}
  
findTask("IamTheWalrus")

/** My 2-step methodology for making your functions a little less partial:
 
1. Awareness. My goal above. Each time you write function, ask yourself 
"How much of the domain type do I really accept? How tightly does the range type describe the output?" Aim to improve both.
 
2. Action. Invent precise types that describe your data. Validate unknown data and tag it with precise types if it passes validation.
Don't think that because your value is a subset of a built-in type, for example expressible by an Int or String, that that's good enough.
Use static typing like you believe in it, goddammit!
*/

/** Lets use Probability as a case study of how to do this in scala. We can model Probability as a floating point number, always in [0, 1]. */

val ProbabilityRange = com.google.common.collect.Ranges.closed[java.lang.Double](0, 1)

/** One appealing approach is to 'tag' an unboxed ('primitive') Double value with a type-level tag designating Probability.
 * Scalaz offers a tag facility, although AFAIK Miles Sabin deserves the credit for first discovering this tagging technique.
 * The tagged type Probability introduces a new subtype of Double. */

import scalaz._
import scalaz.Scalaz._

trait ProbabilityTag
type Probability = Double @@ ProbabilityTag

def p: Probability
assert(p.isInstanceOf[Double])

/** Lets look at 2 ways to go from a Double to a Probability. If we know it won't fail, or don't care if it does, we could simply coerce 
 * a Double into a probability and blowup with an Exception otherwise... */

def asP(p: Double): Probability = {
  validateP(p).getOrElse(throw new IllegalArgumentException(p.toString))
}

/** The above is OK for a small script. But in a library or proper application, its more graceful to validate the Double is a Probability.
 * Think of a validation as a sort into one of two buckets, which can have different types. The right bucket is the success type, 
 * the left bucket is the fail type. 

 * Note that our fail type is Double, not a String error message. I chose this because a fail message can be trivially derived from 
 * the Double value, but its harder to recover the erroneous Double value once its wrapped into a human readable string. 
 * (i.e. String has higher entropy than Double) */

def validateP(p: Double): Validation[Double, Probability] = {
  if (ProbabilityRange.contains(p)) {
    p.asInstanceOf[Probability].success
  }
  else {
    p.fail
  }
}

/** I like to think of the validation step as forming a barrier around a domain model, like the membrane around a cell. 
 * When we pass into the domain, we must get through the validation, but inside we can count on the invariants
 * enforced by the validation holding true. The larger/more complex the domain, the more benefit we gain from having 
 * this membrane since we pay the validation cost only once at the boundary.
 * 
 * To some extent, this is the antithesis of naive "trust no-one defensive programming", as sometimes advocated.
 * We seek efficiency and safety in the domain, by using the type system and validation to concentrate trust checks
 * out at the system boundaries */

/** So now we have a way to construct Probability types. Yay! Lets put them to work and profit - what could possibly go wrong? 
 * 
 * Lets start with a simple method to compute the Probability that two (assumed independent) events will both occur. */

def and_v1(a: Probability, b: Probability): Probability = a * b

/** Type mismatch. Two Doubles multiplied yield a Double, so we popped out of our narrowed type despite knowing that 
 * mathematically, the product of any probabilities must remain within [0, 1] */

def and_v2(a: Probability, b: Probability): Probability = (a * b).asInstanceOf[Probability]

/** Having to stuff our result back into the narrowed type after every operation isn't a very promising future... 
 * 
 * What about dusting off the Numeric typeclass and defining some arithmetic operations over Probability values...? */

implicit val probabilityFractional = Numeric.DoubleIsFractional.asInstanceOf[Fractional[Probability]] 
import Numeric.Implicits._

def and_v3(a: Probability, b: Probability): Probability = a * b

/** No joy. The relation Probability <: Double becomes an impediment here, since Probability 'inherits' Double's
 * implementation of the '*' operator. And when an object /already has/ a method, there's no way to pimp
 * another implementation onto it. 
 * 
 * And v4, which manually invokes the underlying typeclass multiplication method, like v2 is fairly distasteful...*/

def and_v4(a: Probability, b: Probability): Probability = probabilityFractional.times(a, b)

/** Another angle is to box the underlying Double value in a wrapper class. 
 * This gives a "no-compromises" new type, but brings extra memory-overhead.*/

case class Probability2(val p: Double) 

def asP2(p: Double): Probability2 //convert Double => Probability else Exception, just like asP() defined earlier 

/** Lets try to implement the 'and' method for this wrapper type. 
 * 
 * We'll need to implement a custom Numeric typeclass to do multiplication on Probability2 values.
 * This exercise will also demonstrate why splitting Numeric into finer grained pieces might be useful, 
 * as proposed here [https://issues.scala-lang.org/browse/SI-5202]. 
 * Currently, Numeric is like a restaurant which only offers a 10 course banquet. 
 * Oh, well.. lets feast then!  */

implicit val probability2Fractional = new Fractional[Probability2] {
  def plus(x: Probability2, y: Probability2): Probability2 = asP2(x.p + y.p)
  def minus(x: Probability2, y: Probability2): Probability2 = asP2(x.p - y.p)
  def times(x: Probability2, y: Probability2): Probability2 = asP2(x.p * y.p)

  def negate(x: Probability2): Probability2 = ???
  def fromInt(x: Int): Probability2 = ???
  def toInt(x: Probability2): Int = ???
  def toLong(x: Probability2): Long = ???
  def toFloat(x: Probability2): Float = ???
  def toDouble(x: Probability2): Double = ???
  def div(x: Probability2, y: Probability2): Probability2 = ??? 
}

/** Finally! We have lift off....*/
def and_v5(a: Probability2, b: Probability2): Probability2 = a * b

/** And now we can start to compose more complex operations over Probabilities. */
def or(a: Probability2, b: Probability2) = a + b - and_v5(a, b)

/** I want to stress again that the "payback" from introducing specific types get larger
 * the more code you have inside the domain.
 * 
 * Enough on Probability. Lets revisit the other examples I introduced at lines 13-40. */

type NonNegative
def validateNonNegative(n: Double): Validation[Double, NonNegative]
def sqrt(n: NonNegative): NonNegative


type BusinessUnit
type ChargeCode
type Natural <: Int
type Task

def validateBusinessUnit(unitId: String): Validation[String, BusinessUnit]
def validateChargeCode(code: String): Validation[String, ChargeCode]
def validateNatural(taskNum: Int): Validation[Int, Natural]

def findTask(unit: BusinessUnit, code: ChargeCode, taskNum: Natural): Task


/** What about 'reciprocal: Double => Double', ie 1/x ? In this case, the function is defined everywhere except for a single hole
 * or singularity at x == 0. Will it really widely be useful to have */
type NonZero
def validateNonZero(n: Double): Validation[Double, NonZero]
def reciprocal(n: NonZero): NonZero

/** It depends on your problem domain; sometime yes, but sometimes not.
 * 
 * When I was preparing this presentation, I initially thought "Reciprocal not a good clear example, 
 * I should remove it". But I decide to leave it in, because unfortunately, the real world is not black and white
 * but rather shades of grey, and judgment will always be required in deciding whether specific types are warranted.*/


/** Some closing remarks...
 * 
 * In the Probability case study, we wanted to associated a new type with a Double value in [0, 1].
 * It isn't particularly easy in Scala ATM. We needed up having to wrap the value in a case class, so
 * type-level concerns ended up causing runtime impacts.
 * 
 * Haskell has a 'newtype' keyword which does what we wanted: takes an existing data value and 
 * gives it a new, separate type.
 * 
 * At the time of writing, a Inline Classes SIP (Scala Improvement Proposal) is in development 
 * which may offer a future means to introduce newtypes without boxing.
 */
}