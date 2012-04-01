package typestalk

import scala.collection.GenSeqLike
import scala.collection.Parallelizable
import scala.collection.IterableLike
import scala.collection.parallel.immutable.ParSeq

object Content {
  /**
   * Todays talk is part one of two talks about Scala's type system. 
   * I'll do part two in several months time.
   * 
   * I heard it said that Scala's Type system is
   *
   * (a) complex and hard to understand
   * (b) full of advanced features you won't actually need
   *
   * This talk tackles the first point, by explaining Scala's type system. Its not needlessly complex, 
   * but it is deeper and broader than Java's and takes more learning.
   *
   * Once you gain understanding, the "advanced features" turn out to be useful in "real life".
   *
   * IMO appreciation of type systems in mainstream programming is in, or emerging from, 
   * something of a "dark ages" at present,
   * so Scala's type system can seem like alien technology.
   *
   *
   *
   *
   * Q: What is a type system?
   *
   * A: Benjamin Pierce, author of the "standard textbook" Types and Programming Lanugages, offers:
   *
   * "[A type system is a] tractable syntactic method 
   * for proving the absence of certain 
   * program behaviors by classifying phrases
   * according to the kinds of values they compute"
   *
   *
   * Translate "phrases" as "pieces of your program".
   * I like the idea of "classifying pieces of your program according to 
   * the kinds of values they compute"...
   *
   * ...but types do more than "proving the absence of certain program behaviors":
   *
   * - IDE Navigation, Code-completion, refactoring tools
   * - Type Classes use types to dispatch data to the correct function implementation.
   *
   *
   * My 2c:
   *
   * - Types enable you, the programmer, to write much more semantically rich code
   * - Type Systems are governed by mathematics and logic. If your code type checks, 
   * the checker has proven your code has logical consistency in the language of your types.
   *
   *
   *
   *
   * Q: Where do types live?
   *
   * A: In the compiler, at compile time.
   *
   * Not in the running program. They can be erased ('forgotten') at runtime 
   * without changing the execution*.
   *
   * Thus the popular term "dynamically typed" is a misnomer. 
   * People confuse the use of object with "type-like" class pointers in the heap,
   * (ie an implementation technique) which are "type-like", with the types that 
   * described or categorized that class at compile-time.
   *
   *
   * Q: What about when you want to reflect over types at runtime?
   *
   * A: Type erasure will not allow you to reflect over (ie query) your types at runtime.
   *
   * When you do want to reflect over the types in your program at runtime, you can 
   * ask the compiler to 'bottle' any type T into a Manifest[T],
   * which is a descriptor that's retained at runtime.
   *
   * BTW when we use eg the java.lang.reflect.* APIs, we're not strictly recovering 
   * the types at runtime. We're discovering the classes and interfaces
   * that heap objects are associated with.
   *
   *
   *
   *
   *
   * Every class or trait declaration introduces a type of the same name, but they're 
   * not strictly the same thing. 
   * The class is an implementation construct; it's type is a label or classification 
   * that describes it, or any subtype.
   */

  class Example1 {
    def text: String = "example"
  }
  val example1: Example1 = new Example1
  val example2: AnyRef = example1
  val example3: { def text: String } = example1

  /**
   * When we annotate /terms/ like 'example1' with /types/ like Example1 or AnyRef 
   * using the :, this is called type /ascription/.
   *
   * The alternative to ascription is type /inference/, sometimes called type /reconstruction/, 
   * where the compiler infers a
   * term's type from its context.
   */

  val exampleInference = example1

  /**
   * example3 above is a /structural/ type. Structural types directly describe the 
   * features or signatures
   * (eg the 'text' method) that are possessed by the values they classify. 
   * Any value that has a 'text: String'
   * method can be classified using this type.
   *
   * They are the most fundamental way of representing types and the 
   * most mathematically straightforward.
   *
   * The other two are /nominal/, or name-based types, which are what most of us are familiar with. 
   * Nomimal system check types based upon the declared names and subtype relations, 
   * rather than checking if the actual features of the types match.
   * Nominal typing is used in Java, C#, C++ in part because it eases the implementation; 
   * checking if nominal types match is simpler than for structural types.
   */
  trait Interface { def text: String }
  //type mismatch;  found   : typestalk.Content.Example1  required: typestalk.Content.Interface
  val exampleNominalTypingClash: Interface = example1

  /**
   * Do understand that the nominal/structural difference is deeper than syntax. 
   * We can give structural types a name via aliasing, but they remain structural. 
   */

  type Textual = { def text: String; val a: Int }
  val exampleStructuralAlias: Textual = example1

  /**
   * Structural types are written syntactically as a series of members inside curly
   * brackets, and hence resemble the body of a class or trait, but occur where a type is expected.
   *
   * Quiz: what common Java interface has this structural type
   */
  type A[T] = {
    def hasNext: Boolean
    def next: T
    def remove: Unit
  }
  
  /** The current JVM doesn't dispatch methods on structurally typed objects efficiently; 
   * reflective invocation must be used.
   * As a result, Twitter's Effective Scala guidelines presently advise 
   * against using structural typing. 
   * 
   * But keep an open mind for future possibilities. The invoke dynamic bytecode operator, 
   * introduced in JDK 7, will likely make make structurally typed Scala code efficient 
   * in the not too distant future. 
   * 
   * Finally, you may have heard the saying: 
   * "If it looks like a duck, and it walks like a duck, then it is a duck".
   * Its often cited as a limitation with static typing, but in fact its a limitation 
   * of nominal typing.  Structural typing is the essence of what that saying means.*/
  
  
  /**
   * Next topic. /Parametric polymorphism/, aka Generics, and /type parameters/. 
   * In type theory, generic types are sometimes called /Universal Types/ because they 
   * are related to the universal quantifier, "for all", in logic (the upside down A).
   *
   * We often encounter cases where we want to do the same operation, or construct the 
   * same data structure, over different types of data values:
   */

  def applyTwiceString(value: String)(f: (String) => String): String = f(f(value))
  def applyTwiceInt(value: Int)(f: (Int) => Int): Int = f(f(value))

  def applyTwice[A](value: A)(f: (A) => A): A = f(f(value))

  applyTwice("foo")("*" + _ + "*")
  applyTwice(42)(_ / 2)

  /**
   * The symbol 'A' in applyTwice is a /type parameter/. We can think of as method 
   * having two types of possible parameters;
   * a list of Types in square brackets, 
   * followed by one of more lists of Values in round brackets.
   *
   * Keep in mind though, that the type parameters are passed at compile-time, 
   * once at each /call-site/, while the value parameters are passed at runtime. 
   * (a call-site is each place in the code where a method is referred to)
   *
   * For example, how many times are (a) type parameters, and (b) value parameters, 
   * passed to the applyTwice method in the example below?
   */

  1.to(3) { applyTwice(42)(_ / 2) } //1   3
  val applyTwicer = (s: String) => applyTwice(s)("=*=" + _ + "=*=") //1 0
  applyTwicer("ear") //0 1
  applyTwicer("wig") //0  1

  /**
   * Like implicit parameters, type parameters may not appear explictly when they 
   * have been passed by type inference, as in the example above. 
   * We can of course write them explictly; this is sometimes needed when the inferencer fails.
   */

  1.to(3) { applyTwice[Int](42)(_ / 2) }

  /**
   * A brief aside on type inference in Scala. It was no accident that applyTwice 
   * uses currying of its params, ie 2 param lists.
   * I did this to assist the type inferencer. 
   * Look what happens when we don't curry the params
   */

  def applyTwiceUncurried[A](value: A, f: (A) => A): A = f(f(value))

  applyTwiceUncurried(42, _ / 2)
  applyTwiceUncurried[Int](42, _ / 2)
  applyTwiceUncurried(42, (_: Int) / 2)

  /**
   * What's going on here? Well, the inferencer works left to right. 
   * In the curried version,  the parameter in the first list
   * determines the type of A, then the second list follows it. 
   * When Scala evaluates the function literal, eg '_ / 2', type param A
   * is fixed at Int, so it can infer the type of the placeholder '_'.
   */

  /**
   * Now back to type parameters. We've seen examples of /declaring/ a type parameter, 
   * where we define a method, and /passing/ a type parameter,
   * at the call-site where we called the method.
   *
   * As well as method-scope, we can give type params class-scope, 
   * so that all members of the class/trait can refer to the type.
   * Here's an example of declaring and passing a class-level type, 
   * of mixing class- and method- level type params:
   */
  trait Bird extends Ordered[Bird]
  class Hen extends Bird
  class Dove extends Bird

  case class Triple[A](a: A, b: A, c: A)

  case class Pair[A](a: A, b: A) {

    /**
     * More parametric polymorphism in Scala: Type Bounds.
     *
     * The type parameter to the add method below, X, has a constraint (or /lower bound/) 
     * that it be a supertype of A, ie 'X >: A'.
     * In english, this tells us that the add method returns a Triple whose the type is the same, 
     * or more general, than the type held in the pair. 
     * So we can add any type to pair to make the triple, but if they are not homogeneous,
     * the type of the pair will widen as a result.
     *
     * When the Dove is added to the pair above, Scala picks the Least Upper Bound (LUB) type, 
     * so value 'tripleBirds' has type 'Bird'.
     */

    def add[X >: A](c: X) = new Triple[X](a, b, c)

    val pairHens = Pair(new Hen, new Hen)
    val tripleBirds = pairHens.add(new Dove)
    
    /** Quiz: Why does this work? */
    
    class SillyHen extends Hen
    val tripleHens: Triple[Hen] = pairHens.add(new SillyHen)

    /**
     * The combine method demonstrates /upper bounds/. Eg 'B <: A' reads as B must be a subtype of A.
     *  While not very practical, the combine method allows us to combine a Pair with another 
     *  Pair of equal or more specific type, to form a List[A].
     */
    def combine[B <: A](other: Pair[B]) = List[A](a, b, other.a, other.b)

    val pairBirds = Pair(new Dove, new Hen)
    val listBirds = pairBirds.combine(pairHens)

    /**
     * The less method demonstrates combining lower and upper bounds in a single use case. We can ask if another pair is less than
     *  this pair, if the other pair is a supertype and its Ordered.
     */
    def less[X >: A <: Ordered[X]](other: Pair[X]) = other.a < a && other.b < b

    pairHens.less(pairBirds)
  }
  /**
   * In practice, I don't find bounds especially common or useful in day-to-day Scala coding, 
   * as I make limited use of inheritance hierarchies. 
   *  The 'less' example I had to look far and wide to find, and eventually dug up an Odersky paper 
   *  from 2004. But their occasionally useful and  you need to be able to parse them when 
   *  reading other peoples code.
   */

  /**
   * There's two other kinds of bounds, View Bounds and Context Bounds, 
   * which Im going to mention briefly.
   *
   *  The example below uses a View Bound '<%', meaning 
   *  "T can be implicitly converted to Ordered[T]". 
   *  The method returns the maximum element in a collection,
   *   wrapped in an Option in case the collection is zero length.
   *  (reduceOption is a simplified fold that doesn't need a starting value specified.)
   */

  def max1[T <% Ordered[T]](xs: Iterable[T]) = xs.reduceOption((a, b) => { if (a > b) a else b })

  /**
   * Here's the same method done with a Context Bound ':'
   *
   * An Ordering context bound on a type T signifies that an implicit parameter 
   * of type Ordering[T] is being passed.
   * (The import statement allows any type T to be treated as Ordered[T] 
   * if an Ordering[T] is avaialble.)
   *
   * Context bounds are a key syntactic sugar for making type classes workable in Scala. 
   * I hope to show more examples in a future talk on type classes. 
   * I prefer Context Bounds over View Bounds myself.
   */

  import Ordered.orderingToOrdered
  def max2[T: Ordering](xs: Iterable[T]) = xs.reduceOption((a, b) => { if (a > b) a else b })

  
  /**
   * Im now going to overview some of the other varied pieces of
   * Scala's type system, so that you can recognize them syntactically when you encounter them.
   */
  
  /** You've probably seen the 'with' keyword used for mixin composition
   * when defining a class, trait or object. Eg from Scala collections: */
  
  trait SeqLike[+A, +Repr] extends IterableLike[A, Repr] 
    with GenSeqLike[A, Repr] 
    with Parallelizable[A, ParSeq[A]] 
  
  /** 'with' can be used in a type-level context to build compound /types/. 
   * A compound type has the types of all its components. */
  
  trait UnitLength
  case class Vector2[T: Numeric](x: T, y: T) {
    
    def direction: Vector2[T] with UnitLength = ???
  }
  
  /** The type keyword can be used to define type aliases, type-level functions, and type members. 
   * 
   * For example, we could alias the compound type in the previous example 
   * by defining a "type-level function" taking one type parameter T */
  
  type UnitVector[T] = Vector2[T] with UnitLength
  

  /** (Abstract) Type Members allow Classes, Traits and Objects to define types as abstract members, 
   * like slots to be filled in by subclasses. (In fact, the type keyword must always
   * be used within some context, not at top level).
   * 
   * I don't use or understand abstract type members thoroughly myself, so I'm just going to offer
   * the example from Martin Odersky's book: */
  
  trait Food
  class Grass extends Food
  class Fish extends Food
  
  abstract class Animal {
    type SuitableFood <: Food
    def eat(food: SuitableFood)
  }
  class Cow {
    type SuitableFood = Grass
    def eat(food: SuitableFood)
  }
  val cow = new Cow()
  cow.eat(new Grass())
  cow.eat(new Fish())
  
  /** Generally, anything that can be modeled using abstract type members can 
   * also be represented using parametric polymorphism,
   * but possibly less elegantly, ie with more notational overhead. */

  class CowV2[F <: Food] {
    def eat(suitableFood: F)
  }
  
  /** When using type members in contexts, the /type projection/ operator '#' can 
   * be used to refer to types inside a context: */
    
  val grass: Cow#SuitableFood = new Grass
  
  /** One place you might encounter the '#' operator is the "type lamda" idiom, 
   * which lets you curry types instead of values. 
   * 
   * Quiz: Is the syntax we've covered tonight enough to decypher & explain the 
   * return type of Functor.product() below? */
  
  trait Functor[F[_]] {
    
      def product[G[_]](implicit G0: Functor[G]): Functor[({type λ[α] = (F[α], G[α])})#λ]
  }
  
  /** I lifted the above code from Scalaz. I felt it was the most easily grasped 
   * example of the "type lambda" idiom.
  But lets first understand whats its for, then how it works.

  A Functor is a generalized "container". A product is a generalized tuple. 
  So (near-enough) the product method builds a pair of Functors F and G, 
  both containing the same payload type.
 
  However, what the Functors contain has been left unspecified, to be filled in later. 
  Thats the underscore as in F[_] or G[_]. So the return type of the product method 
  is a type function of one argument.

  An inline function is called a lambda expression, 
  so an inline type function is called a type lambda.
  
  Scala doesn't have dedicated syntax for type lambdas, 
  but the syntax above, discovered/invented by Jason Zaugg I think, 
  achieves it through a clever "hack".

 ({type λ[α] = (F[α], G[α])})#λ
 
  Lets parse it out step-by-step, from the left. 
  
  Round brackets are packaging - not sure why they're needed, to be honest, but scalac demands them. 
  Inside we find curly braces, which in a type expression always means "Structural Type".
  The Structural type defines a single member, a type function called λ ("lambda").
  The type function has one parameter 'a'.
  The function returns a tuple of two functors (containers) of as: (F[α], G[α])
  Then, the # type projection operator grabs the result type of λ and "projects", ie returns, it.
  Voila - inline type function!
   
  For comparison, lets write the same lambda at the value level: 
*/
  class A1
  def F(a: A1) = ???
  def G(a: A1) = ???
  val λ = (a: A1) => (F(a), G(a))
  
  
  /**
   * Singleton types are types the describe a single specific value, rather than a category of values.
   * They are written 'x.type', where x is a value:
   */

  val hello = "Hello"
  type H = hello.type
  val goodbye: H = "Goodbye"

  /**
   * Here's an example of where Singleton types can be useful.
   * See how the Builder inheritance heirarchy below has a problem: 
   * use the builder methods in the wrong order and it won't compile:
   */

  class PersonBuilder {
    def build() = ???
    def withName(name: String): PersonBuilder = this
  }
  class ProgrammerBuilder extends PersonBuilder {
    def likesScala(b: Boolean): ProgrammerBuilder = this
  }

  new ProgrammerBuilder().likesScala(true).withName("Ben").build()

  new ProgrammerBuilder().withName("Ben").likesScala(true).build()

  /** Singleton types enable methods to indicate they return the 'this' pointer: */

  class PersonBuilderV2 {
    def build() = ???
    def withName(name: String): this.type = this
  }
  class ProgrammerBuilderV2 extends PersonBuilderV2 {
    def likesScala(b: Boolean): this.type = this
  }

  new ProgrammerBuilderV2().withName("Ben").likesScala(true).build()

  
  /** Before we finish, I want to mention an alternative perspective on subtyping. 
   * If one type is a subtype of another, it means that we're free to 
   * implicitly use or view the subtype as the supertype where ever we wish */
  
  class Supertype(a: String)
  class Subtype(a: String, b: String) extends Supertype(a)
  
  val value: Supertype = new Subtype("some", "params")
  
  /** So the declaration above has established that 'Subtype' is implicitly viewable as a 'Supertype' 
   * anywhere in our present or future code. 
   * 
   * But we can also do this without subtyping being part of our type system, 
   * via implicit conversions: */
  
  class Supertype2(a: String)
  class Subtype2(val a: String, b: String)
  {
  implicit def sub2super(s: Subtype2) = new Supertype2(s.a)
  
  val value2: Supertype2 = new Subtype2("some", "params")
  }
  /** So there is a paralell between explicit supertypes and types that are 
   * implicitly convertible to.
   * 
   * Its interesting to consider the /differences/ too. The implicit conversion has some syntactic
   * overhead, but it also has big benefit: it effect is scoped to a context we can control, 
   * rather than being a global declaration have to make at class definition time.
   * 
   * 
   * This means that your type hierarchy can have different shapes and forms at 
   * different places and times.
   * There is no need to design a "correct" class hierarchy upfront. 
   * It can change and adapt to your needs.
   * 
   * It was precisely this flexibility that enables Scalaz to extend Scala 
   * from outside the core. 
   * When I first discovered Scalaz, with my "OO googles" on, 
   * I thought that "properly", the core Scala libraries ought to be modified; 
   * eg that collections should inherit from eg Monoid and Functor. */
  
  import scalaz._
  import scalaz.Scalaz._
  
  List(1, 2, 3) |+| List(4, 5, 6)
  
  /** Note that structural typing also loosens the rigidity of class hierarchies 
   * in another way,  because there is no need to declare supertypes at all. 
   * A type can be viewed as any other type that is a subset of its interface. */
  
  type Sized  = {def size: Int}
  val sized: Sized = scala.collection.immutable.List(1, 2, 3)
  
  /** Note how in structural typing we can use a lightweight type alias 
   * where in nominal typing we need a (heavier) trait,  eg compare above with: */
  
  trait Sized2 { def size: Int}
  
  
}