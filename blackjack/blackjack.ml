open Random;;

(****************************************************************************************************************************)

module Blackjack = struct

  (* score *)

  module Score = struct
    type score = { min:int ; max:int }
    let add s1 s2 =
      let scores = List.sort compare [ s1.min+s2.min ; s1.min+s2.max ; s1.max+s2.min ; s1.max+s2.max ] in
      let scores_filtered = List.filter (fun x -> x<=21) scores in
      let l = List.length scores_filtered in
      if l > 0 then (* at least one possible score under 21 *)
        { min=List.hd scores_filtered ; max=List.hd (List.rev scores_filtered) }
      else (* no possible scores were under 21 *)
        { min=List.hd scores ; max=List.hd (List.rev scores) }
    let is_bust s = s.min>21
    let to_value s = if s.min<=21 then s.min else s.max
  end

  (* card *)

  module Rank = struct
    type rank = int
    let ranks = [| (1 ,11),"Ace"  ; (2 ,2 ),"2"     ; (3 ,3 ),"3"    ; (4,4),"4" ; (5 ,5 ),"5"  ;
                   (6 ,6 ),"6"    ; (7 ,7 ),"7"     ; (8 ,8 ),"8"    ; (9,9),"9" ; (10,10),"10" ;
                   (10,10),"Jack" ; (10,10),"Queen" ; (10,10),"King" |]
    let to_score  r : Score.score = let mm = fst ranks.(r) in { min=fst mm ; max=snd mm }
    let to_value  r : int = Score.to_value (to_score r)
    let to_string r : string = snd ranks.(r)
  end

  module Suite = struct
    type suite = int
    let suites = [| "Hearts" ; "Diamonds" ; "Clubs" ; "Spades" |]
    let to_string s : string = suites.(s)
  end

  module Card = struct
    type card = { rank:Rank.rank ; suite:Suite.suite }
    let get_score c : Score.score = Rank.to_score c.rank
    let get_value c : int = Rank.to_value c.rank
    let to_string c : string = (Rank.to_string c.rank) ^ " of " ^ (Suite.to_string c.suite)
    let print c = print_string (to_string c)
  end

  let rec join_strings ss sep =
    match ss with
    | []         -> ""
    | s::[]      -> s
    | s::s'::ss' -> s ^ sep ^ s' ^ join_strings ss' sep

  let cards_to_string cs = join_strings (Array.to_list (Array.map Card.to_string cs)) ", "

  let index_to_card (i:int) =
    let rank  = i mod 13 in
    let suite = i / 13 in
    ({ rank=rank ; suite=suite } : Card.card)

  let deck_init = Array.init 52 index_to_card

  let shuffle (cs:Card.card array) =
    let rec shuffle' cs i =
      let l = Array.length cs in
      if i = l - 1 then
        cs
      else
        let j = i + ((Random.int l) - i) in
        let x = cs.(j) in
        cs.(i) <- cs.(j);
        cs.(j) <- x;
        shuffle' cs (j+1)
    in shuffle' cs 0

  (* bjstate *)

  type bjstate = {
    mutable deck         : Card.card array;
    mutable player_hand  : Card.card array;
    mutable player_score : Score.score;
    mutable dealer_hand  : Card.card array;
    mutable dealer_score : Score.score;
    mutable winner       : string option
  }

  let print_bjstate state ~hide_dealer:(h:bool) =
    print_endline "––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––";
    Printf.printf "| player | %d | %s \n" (Score.to_value state.player_score) (cards_to_string state.player_hand);
    if h then
      Printf.printf "| dealer | ## | #### \n"
    else
      Printf.printf "| dealer | %d | %s \n" (Score.to_value state.dealer_score) (cards_to_string state.dealer_hand)

  let draw_from state : Card.card =
    let c = state.deck.(0) in
    state.deck <- Array.sub state.deck 1 ((Array.length state.deck) - 1);
    c

  let score_player state (s:Score.score) =
    state.player_score <- Score.add state.player_score s

  let draw_player state (c:Card.card) =
    state.player_hand <- Array.append state.player_hand [|c|]

  let hit_player state =
    let c = draw_from state in
    score_player state (Card.get_score c);
    draw_player  state c

  let score_dealer state (s:Score.score) =
    state.dealer_score <- Score.add state.dealer_score s

  let draw_dealer state (c:Card.card) =
    state.dealer_hand <- Array.append state.dealer_hand [|c|]

  let hit_dealer state =
    let c = draw_from state in
    score_dealer state (Card.get_score c);
    draw_dealer  state c

  (* moves *)

  type move = Stay | Hit

  let rec get_player_move () : move =
    Printf.printf "–> "; flush stdout;
    let input = read_line () in
    if      String.contains input 's' then Stay
    else if String.contains input 'h' then Hit
    else begin (* unrecognized move *)
      print_endline "[!] unrecognized move. moves are: (s)tay or (h)it";
      get_player_move ()
    end

  let rec play_player state =
    print_bjstate state ~hide_dealer:true;
    let move = get_player_move () in
    match move with
    | Stay -> (* stay *)
        ()
    | Hit -> (* hit *)
        hit_player state;
        if Score.is_bust state.player_score then (* player busted *)
          state.winner <- Some "dealer"
        else
          play_player state

  let rec play_dealer state : unit =
    let player_value = Score.to_value state.player_score in
    let dealer_value = Score.to_value state.dealer_score in
    if (dealer_value>=17) && (player_value<=21) then (* stay *)
      print_bjstate state ~hide_dealer:false
    else (* hit *)
      print_bjstate state ~hide_dealer:false;
      hit_dealer state;
      if Score.is_bust state.dealer_score then (* dealer busted *)
        state.winner <- Some "player"
      else begin
        play_dealer state
      end

  let check_winner state =
    print_endline "––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––";
    if (Score.to_value state.player_score) > 21 then (* player busted *)
      print_endline "[*] player busted and loses :("
    else if (Score.to_value state.dealer_score) > 21 then (* dealer busted *)
      print_endline "[*] dealer busted and player wins!"
    else if (Score.to_value state.player_score) > (Score.to_value state.dealer_score) then
      print_endline "[*] player wins!"
    else
      print_endline "[*] player loses :("

  let rec get_play_again () : bool =
    print_string "[?] play again? "; flush stdout;
    let input = read_line () in
    if      String.contains input 'y' then true
    else if String.contains input 'n' then begin
      print_endline "[*] thank you for playing Blackjack, come again soon.";
      false
    end else begin (* unrecognized response *)
      print_endline "[!] unrecognized response. responses are: (y)es or (n)o";
      get_play_again ()
    end

  let reset state =
    state.deck         <- deck_init;
    state.player_hand  <- [||];
    state.player_score <- { min=0 ; max=0 };
    state.dealer_hand  <- [||];
    state.dealer_score <- { min=0 ; max = 0 };
    state.winner       <- None;
    ()

  let rec play state =
    print_endline "============================================";
    print_endline "||    >-<>-       Blackjack      -<>-<    ||";
    print_endline "============================================";
    hit_player state; hit_player state;
    hit_dealer state; hit_dealer state;
    play_player state;
    print_bjstate state ~hide_dealer:true;
    if state.winner = None then begin (* player did not bust *)
      play_dealer state;
      print_bjstate state ~hide_dealer:false
    end;
    check_winner state;
    if get_play_again () then begin
      reset state;
      play state
    end else ()

end;;

open Blackjack

let main () =
  Random.self_init ();
  let state = { deck         = deck_init;
                 player_hand  = [||];
                 player_score = { min=0 ; max=0 };
                 dealer_hand  = [||];
                 dealer_score = { min=0 ; max=0 };
                 winner       = None } in
  play state
;;

main ()
