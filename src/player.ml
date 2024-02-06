open Card

type hand = {
  card1 : Card.card;
  card2 : Card.card;
}

type state = {
  hand : hand;
  money : int;
}

let create_hand card1_string card2_string =
  { card1 = card1_string; card2 = card2_string }

type decision =
  | Fold
  | CheckCall
  | Raise of int

let rec cards_to_ints all_cards =
  match all_cards with
  | [] -> []
  | h :: t -> [ convert_value_to_int h ] @ cards_to_ints t

let get_cards hand = (hand.card1, hand.card2)
let first_card hand = hand.card1
let second_card hand = hand.card2
