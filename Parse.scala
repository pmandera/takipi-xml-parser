case class Tag(lemma: String, pos: String) {
  lazy val splPos = pos.split(':')
  lazy val aglt = splPos(0) == "aglt"
}

case class TaggedToken(spelling: String, tag: Tag) {
  def tabString = spelling + "\t" + tag.pos + "\t" + tag.lemma
  lazy val aglt = tag.aglt
}


object Parser {
  def parse(fname: String) = {
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

  def agltFilter(t: List[TaggedToken]): List[TaggedToken] = {
    // optimized for tail recursion
    @scala.annotation.tailrec
    def iter(t: List[TaggedToken], acc: List[TaggedToken]): List[TaggedToken] = t match {
      case Nil => acc
      case x :: xs if x.aglt => iter(xs.tail, TaggedToken(xs.head.spelling + x.spelling, xs.head.tag) :: acc)
      case x :: xs => iter(xs, x :: acc)
    }
    iter(t, Nil)
  }

  def filterByc(tagged: List[TaggedToken]): List[TaggedToken] = agltFilter(tagged.reverse) 
}

object Parse extends App {
  val sourceDir = "../toy/tagged/"
  val targDir = "../toy/tagged/parsed/"
  val files = new java.io.File(sourceDir).listFiles map (_.getName)

  files.par foreach { e => save(targDir + e, Parser.filterByc(Parser.parse(sourceDir + e))) }
  // val a = files map { e => Parser.filterByc(Parser.parse(sourceDir + e)) } foreach { println _ }

  def save(fname: String, tagged: Seq[TaggedToken]) = {
     val p = new java.io.PrintWriter(fname)
     tagged foreach { e => p.println(e.tabString) }
     p.close()
  }
}
