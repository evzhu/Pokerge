(** Representation of the commands.

This module represents the commands the user will use to play the Poker game, which
include play, check, bet, fold, and call. It processes and completes each
user request. *)

type object_word = string list
(** The type [object_word] represents the player command. *)

(** The type [command] represents a player command that is composed of word(s):
   fold, check/call, bet with the amount, and quit. *)
type command =
 | Fold
 | CheckCall
 | Bet
 | AllIn
 | Quit
 
exception Empty
(** Raised when an empty command is parsed. *)
exception Malformed
(** Raised when a malformed command is parsed. *)

val parse : string -> command
(** [parse str] parses a player's input (ignores case) into a [command], as follows.

   - [parse "    caLL   "] is [CheckCall]
   - [parse "BeT  20   "] is [Bet 20]

   Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
   characters (only ASCII character code 32; not tabs or newlines, etc.).

   Raises: [Empty] if [str] is the empty string or contains only spaces.

   Raises: [Malformed] if the command is malformed. A command is malformed if
   it is not one of the prescribed commands. *)
