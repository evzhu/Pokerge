(** Abstract logic behind any bot at the table *)

type hand = {
  card1 : Card.card;
  card2 : Card.card;
}
(**Type hand representes a pair of cards*)

(**Type decision reperesents what calls a bot can make*)
type decision =
  | No
  | Fold
  | CheckCall
  | Raise of int

val make_decision : hand -> Card.card list -> int -> decision
(**[make_decision] takes in a bot hand, card list, and amount of money a bot has
   which it then compares all possible hands and makes a concluding decision on
   how to play the round *)

val create_hand : Card.card -> Card.card -> hand
(**[create_hand] takes two cards and makes a hand*)

val initial_hand_decision : hand -> int -> decision
(**[initial_hand_decision] takes in a hand and makes a decision for how the bot
   should play the round before more card are shown*)

val compareCards : Card.card -> Card.card -> int
(**[compareCards a b] takes in two cards, compares their numeric value, and
   returns -1 of a is less than b, 0, if the two are the same value, and 1 if a
   is greater than b*)

val hand_value : Card.card list -> int
(**[hand_value] takes in a list of 5 cards and returns the value of the hand as
   an integer value.*)

val check_pair : Card.card list -> bool
(**[check_pair] takes in a list of 5 cards and returns true of the cards form a
   pair in poker and false otherwise*)

val check_2pair : Card.card list -> bool
(**[check_2pair] takes in a list of 5 cards and returns true of the cards form
   two pairs in poker and false otherwise*)

val check_3kind : Card.card list -> bool
(**[check_3kind] takes in a list of 5 cards and returns true of the cards form a
   three of a kind in poker and false otherwise*)

val check_full_house : Card.card list -> bool
(**[check_full_house] takes in a list of 5 cards and returns true of the cards
   form a full house in poker and false otherwise*)

val check_4kind : Card.card list -> bool
(**[check_4kind] takes in a list of 5 cards and returns true of the cards form a
   4 of a kind in poker and false otherwise*)

val check_straight : Card.card list -> bool
(**[check_straight] takes in a list of 5 cards and returns true of the cards
   form a straight in poker and false otherwise*)

val check_flush : Card.card list -> bool
(**[check_flush] takes in a list of 5 cards and returns true of the cards form a
   flush in poker and false otherwise*)

val check_straight_flush : Card.card list -> bool
(**[check_straight_flush] takes in a list of 5 cards and returns true of the
   cards form a straight flush in poker and false otherwise*)
