package tonivade.poker

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HandSpec extends AnyFlatSpec with Matchers {
  
  "Hand with all different cards" should "be a HighCard" in {
    val hand = FullHand(Card(Spades, Seven), Card(Clubs, Ace), Card(Hearts, Two), Card(Diamonds, Three), Card(Spades, Four))

    hand.hands should be (List(HighCard))
  }
  
  "Hand with two Aces" should "be a Pair" in {
    val hand = FullHand(Card(Spades, Ace), Card(Clubs, Ace), Card(Hearts, Two), Card(Diamonds, Three), Card(Spades, Four))

    hand.hands should be (List(Pair, HighCard))
  }
  
  "Hand with two Aces and two Kings" should "be a TwoPairs" in {
    val hand = FullHand(Card(Spades, Ace), Card(Clubs, Ace), Card(Hearts, King), Card(Diamonds, King), Card(Spades, Four))

    hand.hands should be (List(TwoPairs, HighCard))
  }
  
  "Hand with three Aces" should "be a ThreeOfAKind" in {
    val hand = FullHand(Card(Spades, Ace), Card(Clubs, Ace), Card(Hearts, Ace), Card(Diamonds, Three), Card(Spades, Four))

    hand.hands should be (List(ThreeOfAKind, HighCard))
  }
  
  "Hand with four Aces" should "be a FourOfAKind" in {
    val hand = FullHand(Card(Spades, Ace), Card(Clubs, Ace), Card(Hearts, Ace), Card(Diamonds, Ace), Card(Spades, Four))

    hand.hands should be (List(FourOfAKind, HighCard))
  }
  
  "Hand with consecutive cards" should "be a Straight" in {
    val hand = FullHand(Card(Spades, Ace), Card(Clubs, Two), Card(Hearts, Three), Card(Diamonds, Four), Card(Spades, Five))

    hand.hands should be (List(Straight, HighCard))
  }
  
  "Hand with three Aces and two Kings" should "be a FullHouse" in {
    val hand = FullHand(Card(Spades, Ace), Card(Clubs, Ace), Card(Hearts, Ace), Card(Diamonds, King), Card(Spades, King))

    hand.hands should be (List(FullHouse, ThreeOfAKind, Pair, HighCard))
  }
  
  "Hand with cards of same suit" should "be a Flush" in {
    val hand = FullHand(Card(Spades, Ace), Card(Spades, Three), Card(Spades, Five), Card(Spades, Six), Card(Spades, Ten))

    hand.hands should be (List(Flush, HighCard))
  }
  
  "Hand with consecutive cards of same suit" should "be a StraightFlush" in {
    val hand = FullHand(Card(Spades, Ace), Card(Spades, Two), Card(Spades, Three), Card(Spades, Four), Card(Spades, Five))

    hand.hands should be (List(StraightFlush, Flush, Straight, HighCard))
  }
  
  "Hand with consecutive cards of same suit starting with 10" should "be a RoyalFlush" in {
    val hand = FullHand(Card(Spades, Ace), Card(Spades, Ten), Card(Spades, Jack), Card(Spades, Queen), Card(Spades, King))

    hand.hands should be (List(RoyalFlush, StraightFlush, Flush, Straight, HighCard))
  }
}