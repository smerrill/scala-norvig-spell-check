import scala.io.Source

object NorvigSpellChecker {
  val alphabet = "abcdefghijklmnopqrstuvwxyz"

  def train(text:String) = {
    "[a-z]+".r.findAllIn(text).foldLeft(Map[String, Int]() withDefaultValue 1) {(a, b) => a(b) = a(b) + 1}
  }

  val NWORDS = train(Source.fromFile("big.txt").getLines.mkString.toLowerCase)

  def known(words:Set[String]) = {println("known %s" format words); Set.empty ++ (for(w <- words if NWORDS contains w) yield w)}

  def edits1(word:String) = {
    Set.empty ++
    (for (i <- 0 until word.length) yield (word take i) + (word drop (i + 1))) ++ // Deletes
    (for (i <- 0 until word.length - 1) yield (word take i) + word(i + 1) + word(i) + (word drop (i + 2))) ++ // Transposes
    (for (i <- 0 until word.length; j <- alphabet) yield (word take i) + j + (word drop (i+1))) ++ // Replaces
    (for (i <- 0 until word.length; j <- alphabet) yield (word take i) + j + (word drop i)) // Inserts
  }

  def known_edits2(word:String) = {Set.empty ++ (for (e1 <- edits1(word); e2 <- edits1(e1) if NWORDS contains e2) yield e2)}

  def correct(word: String) = {
    import Stream._

    val str = cons(known(Set(word)), cons(known(edits1(word)), cons(known_edits2(word), empty)))

    str find { !_.isEmpty } match {
      case Some(candidates) => candidates.reduceLeft { (res, n) => if (NWORDS(res) > NWORDS(n)) res else n }
      case None => word
    }
  }
}

/*
import re, collections

def words(text): return re.findall('[a-z]+', text.lower())

def train(features):
    model = collections.defaultdict(lambda: 1)
    for f in features:
        model[f] += 1
    return model

NWORDS = train(words(file('big.txt').read()))

alphabet = 'abcdefghijklmnopqrstuvwxyz'

def edits1(word):
   s = [(word[:i], word[i:]) for i in range(len(word) + 1)]
   deletes    = [a + b[1:] for a, b in s if b]
   transposes = [a + b[1] + b[0] + b[2:] for a, b in s if len(b)>1]
   replaces   = [a + c + b[1:] for a, b in s for c in alphabet if b]
   inserts    = [a + c + b     for a, b in s for c in alphabet]
   return set(deletes + transposes + replaces + inserts)

def known_edits2(word):
    return set(e2 for e1 in edits1(word) for e2 in edits1(e1) if e2 in NWORDS)

def known(words): return set(w for w in words if w in NWORDS)

def correct(word):
    candidates = known([word]) or known(edits1(word)) or known_edits2(word) or [word]
    return max(candidates, key=NWORDS.get)
*/