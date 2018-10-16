object randoms extends App {

  trait Generator[+T] {

    hitesh =>
    def generate: T

    def map[S](f: T => S): Generator[S] = new Generator[S] {
      def generate = f(hitesh.generate)
    }

    def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
      def generate = f(hitesh.generate).generate
    }
  }

  def single[T](x: T): Generator[T] = new Generator[T] {
    def generate = x
  }

  def choose(lo: Int, hi: Int): Generator[Int] = for (x <- integers) yield lo + x % (hi - lo)

  def oneOf[T](xs: T*): Generator[T] =
    for (idx <- choose(0, xs.length)) yield xs(idx)

  val integers = new Generator[Int] {
    val rand = new java.util.Random

    def generate = rand.nextInt()
  }

  /* val booleans = new Generator[Boolean] {
    def generate = integers.generate > 0
  }*/

  val booleans = for (x <- integers) yield (x > 0)

  /*val pairs = new Generator[(Int, Int)] {
    def generate = (integers.generate, integers.generate)
  }*/

  def pairs[T, U](t: Generator[T], u: Generator[U]) = for {
    x <- t
    y <- u
  } yield (x, y)

  def lists: Generator[List[Int]] = for {
    isEmpty <- booleans
    list <- if (isEmpty) emptyList else nonEmptyList
  } yield list

  def emptyList = single(Nil)

  def nonEmptyList = for {
    head <- integers
    tail <- lists
  } yield head :: tail

  trait Tree

  case class Inner(left: Tree, right: Tree) extends Tree
  case class Leaf(x: Int) extends Tree

  def trees: Generator[Tree] = for {
    isLeaf <- booleans
    tree <- if(isLeaf) siLeaf else nonEmptyTree
  } yield tree

  def siLeaf = for {
    x <- integers
  } yield Leaf(x)

    //  Leaf(integers.generate) <- This wont work because of the absence of Map in the if clause

  def nonEmptyTree = for {
    x <- trees
    y <- trees
  } yield Inner(x, y)

  println(trees.generate)


  def test[T](g: Generator[T], noTimes: Int = 100)(test: T=> Boolean): Unit = {
    for(i <- 0 to noTimes) {
      val value = g.generate
      assert(test(value), "Test failed for " + value)
    }
    println("Test passed " + noTimes)
  }

  test(pairs(lists,lists)) {
    case (x,y) => (x ++ y).length > x.length
  }
}
