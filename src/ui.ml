open State
open Player
open Card

exception UnknownError of string

let print_suit str =
  match str with
  | "Spade" -> "♠"
  | "Clubs" -> "♣"
  | "Hearts" -> "♥"
  | "Diamond" -> "♦"
  | _ -> raise (UnknownError "Wrong value in Suit")

let print_value str =
  match str with
  | "Ace" -> "A"
  | "Two" -> "2"
  | "Three" -> "3"
  | "Four" -> "4"
  | "Five" -> "5"
  | "Six" -> "6"
  | "Seven" -> "7"
  | "Eight" -> "8"
  | "Nine" -> "9"
  | "Ten" -> "T"
  | "Jack" -> "J"
  | "Queen" -> "Q"
  | "King" -> "K"
  | _ -> ""

let hand_format pair =
  match pair with
  | x, y ->
      " ----  ----\n| "
      ^ (x |> get_value |> print_value)
      ^ (x |> get_suit |> print_suit)
      ^ " || "
      ^ (y |> get_value |> print_value)
      ^ (y |> get_suit |> print_suit)
      ^ " |\n|    ||    |" ^ "\n|    ||    |" ^ "\n ----  ----\n"

let river_format lst =
  match lst with
  | [] -> ""
  | [ a; b; c ] ->
      " ----  ----  ----\n| "
      ^ (a |> get_value |> print_value)
      ^ (a |> get_suit |> print_suit)
      ^ " || "
      ^ (b |> get_value |> print_value)
      ^ (b |> get_suit |> print_suit)
      ^ " || "
      ^ (c |> get_value |> print_value)
      ^ (c |> get_suit |> print_suit)
      ^ " |\n|    ||    ||    |" ^ "\n|    ||    ||    |"
      ^ "\n ----  ----  ----\n"
  | [ a; b; c; d ] ->
      " ----  ----  ----  ----\n| "
      ^ (a |> get_value |> print_value)
      ^ (a |> get_suit |> print_suit)
      ^ " || "
      ^ (b |> get_value |> print_value)
      ^ (b |> get_suit |> print_suit)
      ^ " || "
      ^ (c |> get_value |> print_value)
      ^ (c |> get_suit |> print_suit)
      ^ " || "
      ^ (d |> get_value |> print_value)
      ^ (d |> get_suit |> print_suit)
      ^ " |\n|    ||    ||    ||    |" ^ "\n|    ||    ||    ||    |"
      ^ "\n ----  ----  ----  ----\n"
  | [ a; b; c; d; e ] ->
      " ----  ----  ----  ----  ----\n| "
      ^ (a |> get_value |> print_value)
      ^ (a |> get_suit |> print_suit)
      ^ " || "
      ^ (b |> get_value |> print_value)
      ^ (b |> get_suit |> print_suit)
      ^ " || "
      ^ (c |> get_value |> print_value)
      ^ (c |> get_suit |> print_suit)
      ^ " || "
      ^ (d |> get_value |> print_value)
      ^ (d |> get_suit |> print_suit)
      ^ " || "
      ^ (e |> get_value |> print_value)
      ^ (e |> get_suit |> print_suit)
      ^ " |\n|    ||    ||    ||    ||    |"
      ^ "\n|    ||    ||    ||    ||    |" ^ "\n ----  ----  ----  ----  ----\n"
  | h :: t -> raise (UnknownError "Error with river")

let back_of_cards pair =
  match pair with
  | x, y ->
      " ----  ----" ^ "\n|////||////|" ^ "\n|////||////|" ^ "\n|////||////|"
      ^ "\n ----  ----\n"

let concat func = List.fold_left func ""

let display_hands state =
  "\nYour Hand: \n" ^ hand_format (state.player_hand |> get_cards)

let display_bot_hands state =
  "\nBot's Hand: \n" ^ hand_format (state |> get_cards)

let display_river state = river_format state.river
let display_pot state = "\nPot: " ^ string_of_int state.pot ^ "\n"

let display_player_money state =
  "\nYour Money: "
  ^ string_of_int state.player_money
  ^ "\nBot Money: "
  ^ string_of_int state.bot_money
  ^ "\n"

let display_all state =
  display_pot state ^ display_river state ^ display_hands state
  ^ display_player_money state
