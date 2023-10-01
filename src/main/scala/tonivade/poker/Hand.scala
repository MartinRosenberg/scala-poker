package tonivade.poker

sealed trait Hand extends Ordered[Hand] {
  def value: Int
  def eval(hand: FullHand): Boolean
  def compare(that: Hand): Int = this.value - that.value
}
object Hand {
  case object HighCard extends Hand {
    val value = 0
    def eval(hand: FullHand): Boolean = true
  }

  case object Pair extends Hand {
    val value = 1
    def eval(hand: FullHand): Boolean = hand.count.count(_._2 == 2) == 1
  }

  case object TwoPairs extends Hand {
    val value = 2
    def eval(hand: FullHand): Boolean = hand.count.count(_._2 == 2) == 2
  }

  case object ThreeOfAKind extends Hand {
    val value = 3
    def eval(hand: FullHand): Boolean = hand.count.exists(_._2 == 3)
  }

  case object Straight extends Hand {
    val value = 4
    def eval(hand: FullHand): Boolean = hand.conseq == 5
  }

  case object Flush extends Hand {
    val value = 5
    def eval(hand: FullHand): Boolean = hand.sameSuit
  }

  case object FullHouse extends Hand {
    val value = 6
    def eval(hand: FullHand): Boolean =
      hand.count.exists(_._2 == 3) && hand.count.exists(_._2 == 2)
  }

  case object FourOfAKind extends Hand {
    val value = 7
    def eval(hand: FullHand): Boolean = hand.count.exists(_._2 == 4)
  }

  case object StraightFlush extends Hand {
    val value = 8
    def eval(hand: FullHand): Boolean = hand.sameSuit && hand.conseq == 5
  }

  case object RoyalFlush extends Hand {
    val value = 9
    def eval(hand: FullHand): Boolean = hand.sameSuit && hand.conseq == 5 && hand.min == Rank.Ten
  }

  val all: List[Hand] = List(RoyalFlush, StraightFlush, FourOfAKind, FullHouse, Flush, Straight, ThreeOfAKind, TwoPairs, Pair, HighCard)
}

case class FullHand(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card) extends Ordered[FullHand] {
  lazy val ranks: List[Rank] = toList.map(_.rank)
  lazy val suits: List[Suit] = toList.map(_.suit)
  lazy val values: List[Int] = ranks.map(_.ordinal)

  lazy val hands: List[Hand] = Hand.all.filter(_.eval(this))
  lazy val bestHand: Hand = hands.max

  def compare(that: FullHand): Int = this.bestHand compare that.bestHand

  def sameSuit: Boolean = suits.distinct.size == 1
  def count: Map[Rank, Int] = toList.groupBy(_.rank).view.mapValues(_.size).toMap
  def contains(rank: Rank): Boolean = ranks.contains(rank)
  def max: Rank = ranks.max
  def min: Rank = ranks.min
  def conseq: Int = Math.max(conseq(values), conseq(values.map(x => (x + 4) % 13))) + 1
  
  private lazy val toList: List[Card] = card1 :: card2 :: card3 :: card4 :: card5 :: Nil
  private def conseq(list: List[Int]): Int = list.sorted.sliding(2).count(a => a(0) + 1 == a(1))
}