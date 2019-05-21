open Random

(****************************************************************************************************************************)
module Blackjack : sig
(****************************************************************************************************************************)

  type rank = int
  val rank_to_string : rank -> string
  val to_value : rank -> int

  type suite = int
  val suite_to_string : suite -> string

  type card = { rank:rank ; suite:suite }
  val print_card : card -> unit

  module type RANK = sig
  type rank = int
  val to_string : rank -> string
  val to_value  : rank -> int
end

module type SUITE = sig
  type suite = int
  val to_string : suite -> string
end

module type CARD = sig
  type card = { rank:int ; suite:int }
  val print : card -> unit
end

type bjstate = {
  mutable deck          : CARD.card array;
  mutable player_hand   : CARD.card list;
  mutable player_score  : score;
  mutable dealer_hand   : CARD.card list;
  mutable dealer_score  : score;
  mutable finished      : bool
}
val play : bjstate -> bjstate

(****************************************************************************************************************************)
end = struct
(****************************************************************************************************************************)

type suite = int

module Rank = struct
  type rank = int
  let ranks = [|
    1,"Ace" ; 2,"2" ; 3,"3" ; 4,"4" ; 5,"5" ; 6,"6" ; 7,"7" ; 8,"8" ; 9,"9" ; 10,"10" ;
    10,"Jack" ; 10,"Queen" ; 10,"King"
  |]
  let to_value  r = fst ranks.(r)
  let to_string r = snd ranks.(r)
end;;

module Suite = struct
  type suite = int
  let suites = [|
    "Hearts" ; "Diamonds" ; "Clubs" ; "Spades"
  |]
  let to_string s = suites.(s)
end;;

module Card = struct
  type card = { rank:Rank.rank ; suite:Suite.suite }
  let print c = Printf.printf "%s of %s" (Rank.to_string c.rank) (Suite.to_string c.suite)
end;;

let index_to_card i =
  let rank  = i mod 13 in
  let suite = i / 13 in
  ({ rank=rank ; suite=suite } : Card.card)

let deck_init = Array.init 52 index_to_card

let shuffle cs =
  let rec shuffle' cs i =
    let l = Array.length cs in
    if i = l - 1 then
      cs
    else
      let j = i + ((Random.int l) - i) in
      let x = cs.(i) in
      cs.(i) <-  cs.(j);
      cs.(j) <- x;
      shuffle' cs (i+1)
  in shuffle' cs 0

type score = { min:int ; max:int }

type bjstate = {
  mutable deck          : Card.card array;
  mutable player_hand   : Card.card list;
  mutable player_score  : score;
  mutable dealer_hand   : Card.card list;
  mutable dealer_score  : score;
  mutable finished      : bool
}

let draw_from state =
  let l = Array.length state.deck in
  let c = Array.sub state.deck 0 1 in
  state.deck <- Array.sub state.deck 1 (l - 1);
  c

type move = Hit | Stay

let rec get_player_move () = ()
  (* Printf.printf "> "; flush stdout;
  let input = read_line () in
  if String.contains input 'h' then
    Hit
  else if String.contains input 's' then
    Stay
  else
    Printf.printf "unrecognized move, moves are (h)it or (s)tay";
    get_player_move () *)

let play_player state = state
  (* let move = get_player_move () in
  match move with
  | Hit ->
      let c = draw_from state in

  | Stay -> _ *)


let play_dealer state = state

let play state = state

(****************************************************************************************************************************)
end;;
(****************************************************************************************************************************)


Blackjack.play;;

(* let main () =
  print_newline ();
  Random.self_init ();

  let deck = Blackjack.shuffle Blackjack.deck_init in
  let _ = Array.map (fun c -> Blackjack.Card.print c ; print_newline ()) deck in

  print_newline ();
  exit 0;
;;

main ();; *)
