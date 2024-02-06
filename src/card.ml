type card = {
  value : string;
  suit : string;
}

let create_card (value_string : string) (suit_string : string) =
  { value = value_string; suit = suit_string }

let get_suit card = card.suit
let get_value card = card.value

(** Converts value of a card into a string representation of the values meaning*)
let convert_value_to_string card =
  match card.value with
  | "One" | "Two" | "Three" | "Four" | "Five" | "Six" -> "Low"
  | "Seven" | "Eight" | "Nine" | "Ten" -> "High"
  | "Jack" | "Queen" | "King" | "Ace" -> "Face"
  | _ -> "None"

let convert_value_to_int card =
  match card.value with
  | "One" -> 1
  | "Two" -> 2
  | "Three" -> 3
  | "Four" -> 4
  | "Five" -> 5
  | "Six" -> 6
  | "Seven" -> 7
  | "Eight" -> 8
  | "Nine" -> 9
  | "Ten" -> 10
  | "Jack" -> 11
  | "Queen" -> 12
  | "King" -> 13
  | "Ace" -> 14
  | _ -> 0
