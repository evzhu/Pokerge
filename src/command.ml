type object_word = string list

type command =
  | Fold
  | CheckCall
  | Bet
  | AllIn
  | Quit

exception Empty
exception Malformed

let parse str =
  let list =
    List.filter
      (fun x -> x <> "")
      (String.split_on_char ' ' (String.lowercase_ascii (String.trim str)))
  in
  match list with
  | [] -> raise Empty
  | [ h ] ->
      if h = "fold" then Fold
      else if h = "check" then CheckCall
      else if h = "call" then CheckCall
      else if h = "quit" then Quit
      else if h = "bet" then Bet
      else if h = "allin" then AllIn
      else raise Malformed
  | _ -> raise Malformed
