open Card
open Bot
open Player

exception SuitError of string
exception CardsError of string

type t = {
  river : Card.card list;
  pot : int;
  player_hand : Player.hand;
  bot_hand : Bot.hand;
  big_blind : string;
  player_money : int;
  bot_money : int;
  previous_bet : int;
  previous_action : string;
  deck : int list;
  turn : string;
}

let deck1 =
  [
    1;
    2;
    3;
    4;
    5;
    6;
    7;
    8;
    9;
    10;
    11;
    12;
    13;
    14;
    15;
    16;
    17;
    18;
    19;
    20;
    21;
    22;
    23;
    24;
    25;
    26;
    27;
    28;
    29;
    30;
    31;
    32;
    33;
    34;
    35;
    36;
    37;
    38;
    39;
    40;
    41;
    42;
    43;
    44;
    45;
    46;
    47;
    48;
    49;
    50;
    51;
    52;
  ]

let int_to_card a =
  create_card
    (match (a - 1) / 4 with
    | 0 -> "Ace"
    | 1 -> "Two"
    | 2 -> "Three"
    | 3 -> "Four"
    | 4 -> "Five"
    | 5 -> "Six"
    | 6 -> "Seven"
    | 7 -> "Eight"
    | 8 -> "Nine"
    | 9 -> "Ten"
    | 10 -> "Jack"
    | 11 -> "Queen"
    | 12 -> "King"
    | _ -> "")
    (match (a - 1) mod 4 with
    | 0 -> "Spade"
    | 1 -> "Clubs"
    | 2 -> "Hearts"
    | 3 -> "Diamond"
    | _ -> "")

let card_to_int card =
  convert_value_to_int card
  +
  match get_suit card with
  | "Spade" -> 1
  | "Clubs" -> 2
  | "Hearts" -> 3
  | "Diamond" -> 4
  | _ -> 0

let rec remove_from_deck curr_state = function
  | [] -> curr_state
  | h :: t ->
      remove_from_deck
        {
          river = curr_state.river;
          pot = curr_state.pot;
          player_hand = curr_state.player_hand;
          bot_hand = curr_state.bot_hand;
          big_blind = curr_state.big_blind;
          player_money = curr_state.player_money;
          bot_money = curr_state.player_money;
          previous_bet = curr_state.previous_bet;
          previous_action = curr_state.previous_action;
          deck = List.filter (fun x -> x <> h) curr_state.deck;
          turn = curr_state.turn;
        }
        t

let init_state hand1 hand2 =
  {
    river = [];
    pot = 3;
    player_hand = hand1;
    bot_hand = hand2;
    big_blind = "bot";
    player_money = 99;
    bot_money = 98;
    previous_bet = 0;
    previous_action = "";
    deck = deck1;
    turn = "player";
  }

let init_state_fold hand1 hand2 player_money bot_money turn_string =
  {
    river = [];
    pot = 3;
    player_hand = hand1;
    bot_hand = hand2;
    big_blind = "bot";
    player_money;
    bot_money;
    previous_bet = 0;
    previous_action = "";
    deck = deck1;
    turn = turn_string;
  }

let init_round_state hand1 hand2 curr_state big_blind_string player_money_int
    bot_money_int turn_string =
  {
    river = curr_state.river;
    pot = curr_state.pot;
    player_hand = hand1;
    bot_hand = hand2;
    big_blind = big_blind_string;
    player_money = player_money_int;
    bot_money = bot_money_int;
    previous_bet = 0;
    previous_action = "";
    deck = deck1;
    turn = turn_string;
  }

let add_card curr_state card =
  {
    river = curr_state.river @ [ card ];
    pot = curr_state.pot;
    player_hand = curr_state.player_hand;
    bot_hand = curr_state.bot_hand;
    big_blind = curr_state.big_blind;
    player_money = curr_state.player_money;
    bot_money = curr_state.player_money;
    previous_bet = curr_state.previous_bet;
    previous_action = curr_state.previous_action;
    deck = (remove_from_deck curr_state [ card_to_int card ]).deck;
    turn = curr_state.turn;
  }

let add_cards curr_state cards =
  {
    river = curr_state.river @ cards;
    pot = curr_state.pot;
    player_hand = curr_state.player_hand;
    bot_hand = curr_state.bot_hand;
    big_blind = curr_state.big_blind;
    player_money = curr_state.player_money;
    bot_money = curr_state.player_money;
    previous_bet = curr_state.previous_bet;
    previous_action = curr_state.previous_action;
    deck = (remove_from_deck curr_state (List.map card_to_int cards)).deck;
    turn = curr_state.turn;
  }

let rec pick_and_remove lst n =
  if n = 0 then []
  else
    let len = List.length lst in
    let rand = Random.int len + 1 in
    let card = int_to_card rand in
    card :: pick_and_remove (List.filter (fun x -> x <> rand) lst) (n - 1)

let grab_card_on_start () =
  Random.self_init ();
  let deck = deck1 in
  pick_and_remove deck 4

let grab_card curr_state =
  let random_int_card = Random.int (List.length curr_state.deck) + 1 in
  int_to_card random_int_card

let find_highest_card_value cards =
  let highestCardVal = ref 0.0 in
  for i = 0 to List.length cards - 1 do
    if
      float_of_int (Card.convert_value_to_int (List.nth cards i))
      > !highestCardVal
    then
      highestCardVal :=
        float_of_int (Card.convert_value_to_int (List.nth cards i))
  done;
  !highestCardVal /. 100.0

let high_card cards =
  let sortedCards = List.sort Bot.compareCards cards in
  match sortedCards with
  | [ a; b; c; d; e ] -> float_of_int (Card.convert_value_to_int e) /. 100.
  | _ -> raise (CardsError "Error with # of cards")

let check_pair cards =
  let sortedCards = List.sort Bot.compareCards cards in
  match sortedCards with
  | [ a; b; c; d; e ] ->
      if Card.convert_value_to_int a = Card.convert_value_to_int b then
        1.
        +. (float_of_int (Card.convert_value_to_int a) /. 100.)
        +. (float_of_int (Card.convert_value_to_int e) /. 10000.)
        +. (float_of_int (Card.convert_value_to_int d) /. 1000000.)
      else if Card.convert_value_to_int b = Card.convert_value_to_int c then
        1.
        +. (float_of_int (Card.convert_value_to_int b) /. 100.)
        +. (float_of_int (Card.convert_value_to_int e) /. 10000.)
        +. (float_of_int (Card.convert_value_to_int d) /. 1000000.)
      else if Card.convert_value_to_int c = Card.convert_value_to_int d then
        1.
        +. (float_of_int (Card.convert_value_to_int c) /. 100.)
        +. (float_of_int (Card.convert_value_to_int e) /. 10000.)
        +. (float_of_int (Card.convert_value_to_int b) /. 1000000.)
      else if Card.convert_value_to_int d = Card.convert_value_to_int e then
        1.
        +. (float_of_int (Card.convert_value_to_int d) /. 100.)
        +. (float_of_int (Card.convert_value_to_int c) /. 10000.)
        +. (float_of_int (Card.convert_value_to_int b) /. 1000000.)
      else 0.0
  | _ -> raise (CardsError "Error with # of cards")

let check_2pair cards =
  let numPairs = ref 0 in
  let sortedCards = List.sort Bot.compareCards cards in
  let final_val = ref 2.0 in
  match sortedCards with
  | [ a; b; c; d; e ] ->
      if Card.convert_value_to_int d = Card.convert_value_to_int e then
        numPairs := !numPairs + 1;
      final_val :=
        !final_val +. (float_of_int (Card.convert_value_to_int e) /. 100.);
      if Card.convert_value_to_int c = Card.convert_value_to_int d then
        numPairs := !numPairs + 1;
      if !final_val = 2.0 then
        final_val :=
          !final_val +. (float_of_int (Card.convert_value_to_int d) /. 100.)
      else
        final_val :=
          !final_val +. (float_of_int (Card.convert_value_to_int d) /. 10000.);
      if Card.convert_value_to_int b = Card.convert_value_to_int c then
        numPairs := !numPairs + 1;
      if !final_val = 2.0 then
        final_val :=
          !final_val +. (float_of_int (Card.convert_value_to_int c) /. 100.)
      else
        final_val :=
          !final_val +. (float_of_int (Card.convert_value_to_int c) /. 10000.);
      if Card.convert_value_to_int a = Card.convert_value_to_int b then
        numPairs := !numPairs + 1;
      if !final_val = 2.0 then
        final_val :=
          !final_val +. (float_of_int (Card.convert_value_to_int a) /. 100.)
      else
        final_val :=
          !final_val +. (float_of_int (Card.convert_value_to_int a) /. 10000.);
      if !numPairs >= 2 then !final_val else 0.0
  | _ -> raise (CardsError "Error with # of cards")

let check_3kind cards =
  let sortedCards = List.sort Bot.compareCards cards in
  match sortedCards with
  | [ a; b; c; d; e ] ->
      if
        Card.convert_value_to_int a = Card.convert_value_to_int b
        && Card.convert_value_to_int b = Card.convert_value_to_int c
      then 3. +. (float_of_int (Card.convert_value_to_int a) /. 100.)
      else if
        Card.convert_value_to_int b = Card.convert_value_to_int c
        && Card.convert_value_to_int c = Card.convert_value_to_int d
      then 3. +. (float_of_int (Card.convert_value_to_int b) /. 100.)
      else if
        Card.convert_value_to_int c = Card.convert_value_to_int d
        && Card.convert_value_to_int d = Card.convert_value_to_int e
      then 3. +. (float_of_int (Card.convert_value_to_int c) /. 100.)
      else 0.0
  | _ -> raise (CardsError "Issue with number of cards")

let check_full_house cards =
  let sortedCards = List.sort Bot.compareCards cards in
  match sortedCards with
  | [ a; b; c; d; e ] ->
      if
        Card.convert_value_to_int a = Card.convert_value_to_int b
        && Card.convert_value_to_int b = Card.convert_value_to_int c
        && Card.convert_value_to_int d = Card.convert_value_to_int e
      then
        6.
        +. (float_of_int (Card.convert_value_to_int a) /. 100.)
        +. (float_of_int (Card.convert_value_to_int d) /. 10000.)
      else if
        Card.convert_value_to_int a = Card.convert_value_to_int b
        && Card.convert_value_to_int c = Card.convert_value_to_int d
        && Card.convert_value_to_int d = Card.convert_value_to_int e
      then
        6.
        +. (float_of_int (Card.convert_value_to_int c) /. 100.)
        +. (float_of_int (Card.convert_value_to_int a) /. 10000.)
      else 0.0
  | _ -> raise (CardsError "Issue with number of cards")

let check_4kind cards =
  let numOfKind = Array.make 15 0 in
  let sortedCards = List.sort Bot.compareCards cards in
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
      if numOfKind.(Card.convert_value_to_int a) = 4 then
        7. +. (float_of_int (Card.convert_value_to_int a) /. 100.0)
      else if numOfKind.(Card.convert_value_to_int b) = 4 then
        7. +. (float_of_int (Card.convert_value_to_int b) /. 100.0)
      else if numOfKind.(Card.convert_value_to_int c) = 4 then
        7. +. (float_of_int (Card.convert_value_to_int c) /. 100.0)
      else if numOfKind.(Card.convert_value_to_int d) = 4 then
        7. +. (float_of_int (Card.convert_value_to_int d) /. 100.0)
      else if numOfKind.(Card.convert_value_to_int e) = 4 then
        7. +. (float_of_int (Card.convert_value_to_int e) /. 100.0)
      else 0.0
  | _ -> raise (CardsError "Issue with number of cards")

let check_straight cards =
  let sortedCards = List.sort Bot.compareCards cards in
  let sortedValues = List.map Card.get_value sortedCards in
  match sortedValues with
  | [ "Two"; "Three"; "Four"; "Five"; "Ace" ] -> 4. +. 0.05
  | [ "Two"; "Three"; "Four"; "Five"; "Six" ] -> 4. +. 0.06
  | [ "Three"; "Four"; "Five"; "Six"; "Seven" ] -> 4. +. 0.07
  | [ "Four"; "Five"; "Six"; "Seven"; "Eight" ] -> 4. +. 0.08
  | [ "Five"; "Six"; "Seven"; "Eight"; "Nine" ] -> 4. +. 0.09
  | [ "Six"; "Seven"; "Eight"; "Nine"; "Ten" ] -> 4. +. 0.1
  | [ "Seven"; "Eight"; "Nine"; "Ten"; "Jack" ] -> 4. +. 0.11
  | [ "Eight"; "Nine"; "Ten"; "Jack"; "Queen" ] -> 4. +. 0.12
  | [ "Nine"; "Ten"; "Jack"; "Queen"; "King" ] -> 4. +. 0.13
  | [ "Ten"; "Jack"; "Queen"; "King"; "Ace" ] -> 4. +. 0.14
  | [ a; b; c; d; e ] -> 0.0
  | _ -> raise (CardsError "Issue with number of cards")

(*"Spade" | 1 -> "Clubs" | 2 -> "Hearts" | 3 -> "Diamond"*)
let check_flush cards =
  let suits = Array.make 5 0 in
  let sortedCards = List.sort Bot.compareCards cards in
  for i = 0 to List.length sortedCards - 1 do
    match Card.get_suit (List.nth sortedCards i) with
    | "Spade" -> suits.(0) <- suits.(0) + 1
    | "Clubs" -> suits.(1) <- suits.(1) + 1
    | "Diamond" -> suits.(2) <- suits.(2) + 1
    | "Hearts" -> suits.(3) <- suits.(3) + 1
    | _ -> suits.(4) <- suits.(4) + 1
  done;
  if suits.(0) = 5 then 5. +. find_highest_card_value cards
  else if suits.(1) = 5 then 5. +. find_highest_card_value cards
  else if suits.(2) = 5 then 5. +. find_highest_card_value cards
  else if suits.(3) = 5 then 5. +. find_highest_card_value cards
  else if suits.(4) > 0 then
    raise (CardsError "Some card had an impossible suit.")
  else 0.0

let check_straight_flush cards =
  if check_flush cards > 0.0 && check_straight cards > 0.0 then
    let highestCardVal = find_highest_card_value cards in
    8. +. highestCardVal
  else 0.0

let rec combinations k lst =
  if k = 0 then [ [] ]
  else
    let rec inner = function
      | [] -> []
      | h :: t -> List.map (fun z -> h :: z) (combinations (k - 1) t) :: inner t
    in
    List.concat (inner lst)

let hand_value hand =
  let value = ref 0.0 in
  value := high_card hand;
  let pair_value = check_pair hand in
  if pair_value > !value then value := pair_value;
  let twoPairValue = check_2pair hand in
  if twoPairValue > !value then value := twoPairValue;
  let threeKindValue = check_3kind hand in
  if threeKindValue > !value then value := threeKindValue;
  let straightValue = check_straight hand in
  if straightValue > !value then value := straightValue;
  let flushValue = check_flush hand in
  if flushValue > !value then value := flushValue;
  let fullHouseValue = check_full_house hand in
  if fullHouseValue > !value then value := fullHouseValue;
  let fourKindValue = check_4kind hand in
  if fourKindValue > !value then value := fourKindValue;
  let straightFlushValue = check_straight_flush hand in
  if straightFlushValue > !value then value := straightFlushValue;
  !value

let find_winner (bot_hand : Bot.hand) (player_hand : Bot.hand) river =
  let allBotCards = bot_hand.card1 :: bot_hand.card2 :: river in
  let allPlayerCards = player_hand.card1 :: player_hand.card2 :: river in
  let botCardCombos = combinations 5 allBotCards in
  let topBotVal = ref 0.0 in
  for i = 0 to List.length botCardCombos - 1 do
    let curr_value = hand_value (List.nth botCardCombos i) in
    if curr_value > !topBotVal then topBotVal := curr_value
  done;
  let playerCardCombos = combinations 5 allPlayerCards in
  let topPlayerVal = ref 0.0 in
  for i = 0 to List.length playerCardCombos - 1 do
    let curr_value = hand_value (List.nth playerCardCombos i) in
    if curr_value > !topPlayerVal then topPlayerVal := curr_value
  done;
  if !topPlayerVal > !topBotVal then "player"
  else if !topPlayerVal < !topBotVal then "bot"
  else "tie"
