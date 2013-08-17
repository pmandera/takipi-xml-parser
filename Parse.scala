/**
  * Represent PoS tag + lemma
  */
case class Tag(lemma: String, pos: String) {
  lazy val splPos = pos.split(':')
  lazy val aglt = splPos(0) == "aglt"
}

/**
  * Represent token and its tag
  */
case class TaggedToken(spelling: String, tag: Tag) {
  lazy val tabString = spelling + "\t" + tag.pos + "\t" + tag.lemma
  lazy val aglt = tag.aglt
}

/**
  * TAKiPi parser
  */
object Parser {
  /**
    * Return list of TaggedTokens loaded from TaKIPI XML output
    */
  def parse(fname: String): List[TaggedToken] = {
    println("Parsing " + fname)
    val orthre = """<orth>(.*)</orth>.*""".r
    val disambre = """.*disamb.*<base>(.*)</base><ctag>(.*)</ctag>.*""".r
    val data = scala.io.Source.fromFile(fname).mkString
    val orth =  orthre.findAllIn(data).matchData map { e => e.group(1) } 
    val disamb = disambre.findAllIn(data).matchData map { e => Tag(e.group(1), e.group(2)) }
    orth zip disamb map { e => e match {
      case (a, b) => TaggedToken(a, b)
    } } toList
  }

  /**
    * Attach 'agglunitative' morphems to words
    */
  def attachAglt(t: List[TaggedToken]): List[TaggedToken] = {
    // optimized for tail recursion
    @scala.annotation.tailrec
    def iter(t: List[TaggedToken], acc: List[TaggedToken]): List[TaggedToken] = t match {
      case Nil => acc
      case x :: xs if x.aglt => iter(xs.tail, TaggedToken(xs.head.spelling + x.spelling, xs.head.tag) :: acc)
      case x :: xs => iter(xs, x :: acc)
    }
    iter(t.reverse, Nil)
  }
}

object Parse extends App {
  val sourceDir = "../tmp_tagged/"
  val targDir = "../tagged/"
  val files = new java.io.File(sourceDir).listFiles map (_.getName)

  // use parallel collections for speed up
  files.par foreach { e => save(targDir + e, 
      Parser.attachAglt(
        Parser.parse(sourceDir + e)
    )) }

  /**
    * Saves results
    */
  def save(fname: String, tagged: Seq[TaggedToken]): Unit = {
     val p = new java.io.PrintWriter(fname)
     tagged foreach { e => p.println(e.tabString) }
     p.close()
  }
}
