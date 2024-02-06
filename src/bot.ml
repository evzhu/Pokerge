open Card
open Player

exception SuitError of string
exception CardsError of string

type hand = {
  card1 : Card.card;
  card2 : Card.card;
}

let create_hand card1_string card2_string =
  { card1 = card1_string; card2 = card2_string }

type decision =
  | No
  | Fold
  | CheckCall
  | Raise of int

type hand_options =
  | High
  | Pair
  | TwoPair
  | ThreeKind
  | Straight
  | Flush
  | FullHouse
  | FourKind
  | StraightFlush
  | RoyaleFlush

let initial_hand_decision hand money =
  let card1_value = convert_value_to_int hand.card1 in
  let card2_value = convert_value_to_int hand.card2 in
  if card1_value + card2_value >= 20 then Raise (money / 10)
  else if card1_value - card2_value = 0 then Raise (money / 10)
  else if card1_value - card2_value = -1 || card1_value - card2_value = 1 then
    CheckCall
  else if get_suit hand.card1 = get_suit hand.card2 then CheckCall
  else Fold

let rec cards_to_ints all_cards =
  match all_cards with
  | [] -> []
  | h :: t -> [ convert_value_to_int h ] @ cards_to_ints t

(**Need to switch stuff inside array to get int value from card string value*)

(**Need to switch stuff inside array to get int value from card string value*)

(**Need to switch stuff inside array to get int value from card string value*)

let compareCards c1 c2 =
  if Card.convert_value_to_int c2 < Card.convert_value_to_int c1 then 1
  else if Card.convert_value_to_int c2 > Card.convert_value_to_int c1 then -1
  else 0

let check_pair cards =
  let sortedCards = List.sort compareCards cards in
  match sortedCards with
  | [ a; b; c; d; e ] ->
      if Card.convert_value_to_int a = Card.convert_value_to_int b then true
      else if Card.convert_value_to_int a = Card.convert_value_to_int c then
        true
      else if Card.convert_value_to_int a = Card.convert_value_to_int d then
        true
      else if Card.convert_value_to_int a = Card.convert_value_to_int e then
        true
      else if Card.convert_value_to_int b = Card.convert_value_to_int c then
        true
      else if Card.convert_value_to_int b = Card.convert_value_to_int d then
        true
      else if Card.convert_value_to_int b = Card.convert_value_to_int e then
        true
      else if Card.convert_value_to_int c = Card.convert_value_to_int d then
        true
      else if Card.convert_value_to_int c = Card.convert_value_to_int e then
        true
      else if Card.convert_value_to_int d = Card.convert_value_to_int e then
        true
      else false
  | _ -> false

let check_2pair cards =
  let numPairs = ref 0 in
  let sortedCards = List.sort compareCards cards in
  match sortedCards with
  | [ a; b; c; d; e ] ->
      if Card.convert_value_to_int a = Card.convert_value_to_int b then
        numPairs := !numPairs + 1;
      if Card.convert_value_to_int a = Card.convert_value_to_int c then
        numPairs := !numPairs + 1;
      if Card.convert_value_to_int a = Card.convert_value_to_int d then
        numPairs := !numPairs + 1;
      if Card.convert_value_to_int a = Card.convert_value_to_int e then
        numPairs := !numPairs + 1;
      if Card.convert_value_to_int b = Card.convert_value_to_int c then
        numPairs := !numPairs + 1;
      if Card.convert_value_to_int b = Card.convert_value_to_int d then
        numPairs := !numPairs + 1;
      if Card.convert_value_to_int b = Card.convert_value_to_int e then
        numPairs := !numPairs + 1;
      if Card.convert_value_to_int c = Card.convert_value_to_int d then
        numPairs := !numPairs + 1;
      if Card.convert_value_to_int c = Card.convert_value_to_int e then
        numPairs := !numPairs + 1;
      if Card.convert_value_to_int d = Card.convert_value_to_int e then
        numPairs := !numPairs + 1;
      if !numPairs >= 2 then true else false
  | _ -> false

let check_3kind cards =
  let sortedCards = List.sort compareCards cards in
  match sortedCards with
  | [ a; b; c; d; e ] ->
      if
        Card.convert_value_to_int a = Card.convert_value_to_int b
        && Card.convert_value_to_int b = Card.convert_value_to_int c
      then true
      else if
        Card.convert_value_to_int b = Card.convert_value_to_int c
        && Card.convert_value_to_int c = Card.convert_value_to_int d
      then true
      else if
        Card.convert_value_to_int c = Card.convert_value_to_int d
        && Card.convert_value_to_int d = Card.convert_value_to_int e
      then true
      else false
  | _ -> false

let check_full_house cards =
  let sortedCards = List.sort compareCards cards in
  match sortedCards with
  | [ a; b; c; d; e ] ->
      if
        Card.convert_value_to_int a = Card.convert_value_to_int b
        && Card.convert_value_to_int b = Card.convert_value_to_int c
        && Card.convert_value_to_int d = Card.convert_value_to_int e
      then true
      else if
        Card.convert_value_to_int a = Card.convert_value_to_int b
        && Card.convert_value_to_int c = Card.convert_value_to_int d
        && Card.convert_value_to_int d = Card.convert_value_to_int e
      then true
      else false
  | _ -> false

let check_4kind cards =
  let numOfKind = Array.make 15 0 in
  let sortedCards = List.sort compareCards cards in
  match sortedCards with
  | [ a; b; c; d; e ] ->
      numOfKind.(Card.convert_value_to_int a) <-
        numOfKind.(Card.convert_value_to_int a) + 1;
      numOfKind.(Card.convert_value_to_int b) <-
        numOfKind.(Card.convert_value_to_int b) + 1;
      numOfKind.(Card.convert_value_to_int c) <-
        numOfKind.(Card.convert_value_to_int c) + 1;
      numOfKind.(Card.convert_value_to_int d) <-
        numOfKind.(Card.convert_value_to_int d) + 1;
      numOfKind.(Card.convert_value_to_int e) <-
        numOfKind.(Card.convert_value_to_int e) + 1;
      if numOfKind.(Card.convert_value_to_int a) = 4 then true
      else if numOfKind.(Card.convert_value_to_int b) = 4 then true
      else if numOfKind.(Card.convert_value_to_int c) = 4 then true
      else if numOfKind.(Card.convert_value_to_int d) = 4 then true
      else if numOfKind.(Card.convert_value_to_int e) = 4 then true
      else false
  | _ -> false

let check_straight cards =
  let sortedCards = List.sort compareCards cards in
  let sortedValues = List.map Card.get_value sortedCards in
  match sortedValues with
  | [ "Two"; "Three"; "Four"; "Five"; "Ace" ] -> true
  | [ "Two"; "Three"; "Four"; "Five"; "Six" ] -> true
  | [ "Three"; "Four"; "Five"; "Six"; "Seven" ] -> true
  | [ "Four"; "Five"; "Six"; "Seven"; "Eight" ] -> true
  | [ "Five"; "Six"; "Seven"; "Eight"; "Nine" ] -> true
  | [ "Six"; "Seven"; "Eight"; "Nine"; "Ten" ] -> true
  | [ "Seven"; "Eight"; "Nine"; "Ten"; "Jack" ] -> true
  | [ "Eight"; "Nine"; "Ten"; "Jack"; "Queen" ] -> true
  | [ "Nine"; "Ten"; "Jack"; "Queen"; "King" ] -> true
  | [ "Ten"; "Jack"; "Queen"; "King"; "Ace" ] -> true
  | _ -> false

(*"Spade" | 1 -> "Clubs" | 2 -> "Hearts" | 3 -> "Diamond"*)
let check_flush cards =
  let suits = Array.make 5 0 in
  let sortedCards = List.sort compareCards cards in
  for i = 0 to List.length sortedCards - 1 do
    match Card.get_suit (List.nth sortedCards i) with
    | "Spade" -> suits.(0) <- suits.(0) + 1
    | "Clubs" -> suits.(1) <- suits.(1) + 1
    | "Diamond" -> suits.(2) <- suits.(2) + 1
    | "Hearts" -> suits.(3) <- suits.(3) + 1
    | _ -> suits.(4) <- suits.(4) + 1
  done;
  if suits.(4) > 0 then false
  else if suits.(0) = 5 then true
  else if suits.(1) = 5 then true
  else if suits.(2) = 5 then true
  else if suits.(3) = 5 then true
  else false

let check_straight_flush cards =
  if check_flush cards && check_straight cards then true else false

let rec combinations k lst =
  if k = 0 then [ [] ]
  else
    let rec inner = function
      | [] -> []
      | h :: t -> List.map (fun z -> h :: z) (combinations (k - 1) t) :: inner t
    in
    List.concat (inner lst)

let hand_value hand =
  if check_straight_flush hand then 9
  else if check_4kind hand then 8
  else if check_full_house hand then 7
  else if check_flush hand then 6
  else if check_straight hand then 5
  else if check_3kind hand then 4
  else if check_2pair hand then 3
  else if check_pair hand then 2
  else 1

let make_decision hand river money =
  let allCards = hand.card1 :: hand.card2 :: river in
  let cardCombos = combinations 5 allCards in
  let top_value = ref 1 in
  for i = 0 to List.length cardCombos - 1 do
    let curr_value = hand_value (List.nth cardCombos i) in
    if curr_value > !top_value then top_value := curr_value
  done;
  if !top_value < 2 then Fold
  else if !top_value < 4 then CheckCall
  else Raise (money / 7)
