(*#require "graphics"*)
open Graphics

(* les couleurs des jetons                                 *)
type couleur = Rouge | Jaune

(* les dimensions du plateau de jeu                        *)
let nb_colonnes = 7
let nb_lignes   = 6

(* le plateau des jeux, de dimension nb_lignes * nb_colonnes *)
type game = couleur option array array

(* un état du jeu                                          *)
type etat = { game : game ; joueur : couleur }

let autre = function Rouge -> Jaune | Jaune -> Rouge

(* Quelques fonctions graphiques *)
let color_of_couleur = function Rouge -> Graphics.red | _ -> Graphics.yellow

let cx = 80                     (* nb pixel x case jeu *)
let cy = 80                     (* nb pixel y case jeu *)
let sx = cx * nb_colonnes
let sy = cy * nb_lignes

let draw_game (e: etat) =
  for i = 0 to nb_colonnes do
    Graphics.moveto 0 (i * cx);
    Graphics.lineto sx (i * cx);
  done;
  for j = 0 to nb_lignes   do
    Graphics.moveto (j * cy) 0;
    Graphics.lineto (j * cy) sy;
  done;
  for i = 0 to (nb_colonnes-1) do
    for j = 0 to (nb_lignes  -1) do
      match e.game.(i).(j) with
      | None -> ()
      | Some c -> begin
          Graphics.set_color (color_of_couleur c);
          Graphics.fill_circle (i*cx + cx/2) (j*cy + cy/2) ((min cx cy)/2);
          Graphics.set_color Graphics.black end
    done
  done

(* Boite à outils *)
(* d encode une direction : E, NE, N, NO, O, SO, S, SE
                            0   1  2   3, 4,  5, 6,  7
   on calcule la case obtenue en se déplaçant k fois dans la direction d, depuis la case (i, j)
*)
let rec move (k: int) (i, j) (d: int) =
  match d with
  | 0 -> (i+k, j)               (* E *)
  | 1 -> (i+k, j+k)             (* NE *)
  | 2 -> (i  , j+k)             (* N *)
  | 3 -> (i-k, j+k)             (* NO *)
  | _ -> move (-k) (i, j) (d-4)

(* permet de tester si une case (i, j) est dans le plateau de jeu *)
let is_in (i, j) =
  i >= 0 && j >= 0 && i < nb_colonnes && j < nb_lignes
(* permet de tester si une case (i, j) du plateau g est libre *)
let is_free g (i, j) =
  is_in (i, j) && g.(i).(j) = None
(* permet de tester si une case (i, j) du plateau g contient la couleur c *)
let is_color g (i, j) c =
  is_in (i, j) && g.(i).(j) = Some c

(* retourne une copie de la matrice mm *)
let copy_matrix mm =
  let n = Array.length mm in
  if n = 0 then [||]
  else
    let m = Array.length mm.(0) in
    if m = 0 then Array.init n (fun i -> [||])
    else
      Array.init n (fun i -> Array.copy mm.(i))

(* matrice des valeurs des cases *)
let val_cases =
  [|
    [|3 ; 4 ; 5  ; 7  ; 5  ; 4 ; 3|];
    [|4 ; 6 ; 8  ; 10 ; 8  ; 6 ; 4|];
    [|5 ; 8 ; 11 ; 13 ; 11 ; 8 ; 5|];
    [|5 ; 8 ; 11 ; 13 ; 11 ; 8 ; 5|];
    [|4 ; 6 ; 8  ; 10 ; 8  ; 6 ; 4|];
    [|3 ; 4 ; 5  ; 7  ; 5  ; 4 ; 3|];
  |]

(*fonction heuristique*)
let heuristique (e: etat) (c: couleur): int =
	let sum = ref 0 in
	for i = 0 to (nb_lignes -1) do
		for j = 0 to (nb_colonnes -1) do
			match e.game.(j).(i) with
			|None -> ()
			|Some(a) when a = c -> sum := !sum + val_cases.(i).(j)
			|Some(a) -> sum := !sum - val_cases.(i).(j)
		done
	done; !sum

(*fonction heuristique meillieur*)
let max_array (tab: int array) = (Array.fold_left (fun acc a -> if acc>a then acc else a) tab.(0) tab)


(*print array*)
let print_array (tab : int array) =
	print_string "[|";
	for i = 0 to ((Array.length tab) -2) do
		print_int tab.(i); print_string " ,"
	done; print_int tab.((Array.length tab) -1); print_string "]"; print_newline()

let print_case_value (e: etat) ((i, j): int*int) =
	if (not (is_in (i, j))) then begin print_string "None"; print_string "here" end else
		begin
		if e.game.(j).(i) = None then print_string "None";
		if e.game.(j).(i) = Some(Rouge) then print_string "Rouge";
		if e.game.(j).(i) = Some(Jaune) then print_string "Jaune";
		end

let est_gagnant (e : etat) ((i, j) : int*int) (c: couleur) : bool =
	let nb_pion_array = Array.make 8 0 in
	for k = -3 to 3 do
		if (is_in (j+k, i)) && (is_color e.game (j+k, i) c) then
			begin
			if k >= 0 then nb_pion_array.(0) <- nb_pion_array.(0) + 1;
			if k <= 0 then nb_pion_array.(4) <- nb_pion_array.(4) + 1;
			end;
		if (is_in (j, i+k)) && (is_color e.game (j, i+k) c) then
			begin
			if k >= 0 then nb_pion_array.(1) <- nb_pion_array.(1) + 1;
			if k <= 0 then nb_pion_array.(5) <- nb_pion_array.(5) + 1;
			end;
		if (is_in (j-k, i+k)) && (is_color e.game (j-k, i+k) c) then
			begin
			if k <= 0 then nb_pion_array.(2) <- nb_pion_array.(2) + 1;
			if k >= 0 then nb_pion_array.(6) <- nb_pion_array.(6) + 1;
			end;
		if (is_in (j-k, i-k)) && (is_color e.game (j-k, i-k) c) then
			begin
			if k <= 0 then nb_pion_array.(3) <- nb_pion_array.(3) + 1;
			if k >= 0 then nb_pion_array.(7) <- nb_pion_array.(7) + 1;
			end
	done; (max_array nb_pion_array) >= 4

let better_h (e: etat) (c: couleur): int =
	let sum = ref 0 in
	for i = 0 to (nb_lignes -1) do
		for j = 0 to (nb_colonnes -1) do
			match e.game.(j).(i) with
			|None -> ()
			|Some(a) when a = c ->
				if (est_gagnant e (i, j) a) then (sum := max_int)
				else (sum := !sum + val_cases.(i).(j))
			|Some(a) ->
				if (est_gagnant e (i, j) a) then (sum := min_int)
				else (sum := !sum - val_cases.(i).(j))
		done
	done; !sum

(*print etats*)
let print_etat (e: etat) =
	print_string "le joueur est";
	begin
	match e.joueur with
	|Rouge -> print_string "Rouge";
	|Jaune -> print_string "Jaune";
	end;
	print_newline();
	print_string "[";
	for i = 0 to (nb_colonnes -1) do
		print_string "[";
		for j = 0 to (nb_lignes-2) do
			match e.game.(i).(j) with
			|None -> print_string "None, "
			|Some(a) -> match a with
				|Rouge ->  print_string "Rouge, "
				|Jaune -> print_string "Jaune, "
		done;
		begin
		match e.game.(i).(nb_lignes -1) with
		|None -> print_string "None]";
		|Some(a) -> begin match a with
			|Rouge ->  print_string "Rouge]";
			|Jaune -> print_string "Jaune]";
				end;
		end;
		print_newline();
	done; print_string "l'heauristique est "; print_int (heuristique e Jaune); print_newline()

(* fonctions à compléter, elles sont définies ici pour pouvoir faire typer le reste du programme *)
let place_jeton (e: etat) (i: int): etat option =
	let rec desc (j: int) =
		if (is_in (i, j-1)) && (is_free e.game (i, j-1)) then (desc (j-1))
		else e.game.(i).(j) <- Some(e.joueur)
	in
	if (is_free e.game (i, nb_lignes -1)) then
		begin
		(desc (nb_lignes -1));
		match e.joueur with
		|Rouge -> Some({game = e.game; joueur = Jaune})
		|Jaune -> Some({game = e.game; joueur = Rouge})
		end
	else None

(* la fameuse fonction next*)
let next (e: etat): etat list =
	let rec aux i etatsLists =
		if i = -1 then etatsLists else begin
		match (place_jeton {game = (copy_matrix e.game); joueur = e.joueur} i) with
		|None -> aux (i-1) etatsLists
		|Some(a) ->  a::(aux (i-1) etatsLists)
		end
	in
	let heur = (better_h e e.joueur) in
	if (heur >= (max_int/2)) || (heur <= (min_int/2)) then []
	else (aux (nb_colonnes -1) [])

(*la fameuse fonction minmax*)
let max_liste (liste: int list) = (List.fold_left (fun acc a -> if acc>a then acc else a) (List.hd liste) liste)
let min_liste (liste: int list) = (List.fold_left (fun acc a -> if acc<a then acc else a) (List.hd liste) liste)

let rec minmax (p: int) (e: etat) (j: couleur): int =
	let nexte = (next e) in
	if (nexte = []) then
		begin
		let heur = (better_h e j) in
		if (heur < (max_int/2)) && (heur > (min_int/2)) then 0
		else heur
		end
	else
		begin
		if (p = 0) then (better_h e j) else
			if (j = e.joueur) then (max_liste (List.map (fun a -> minmax (p-1) a j) nexte)) else
			(min_liste (List.map (fun a -> minmax (p-1) a j) nexte))
		end

(*elalage  ausi better minmax*)
exception Return of int

let rec ab_minmax (p: int) (e: etat) (j: couleur) ((a, b): int*int) =
	let nexte = (next e) in
	if (nexte = []) then
		begin
			let heur = (better_h e j) in
			if heur  >= max_int/2 then b
			else if heur <= (max_int/2) then a
			else
				begin
				if b <= 0 then b
				else if a >= 0 then a
				else 0
				end
		end
	else
		begin
		if (p=0) then
			let heur_e_j = (heuristique e j) in
			if heur_e_j >= b then b else
			if heur_e_j <= a then a else
			heur_e_j
		else if (j = e.joueur) then
			begin
			let v = ref a in
			try
			(List.iter (fun ele -> v := (max !v (ab_minmax (p-1) ele j (!v, b))); if !v>=b then raise (Return b)) nexte);
			!v
			with |Return(t) -> t
			end
		else
			begin
			let u = ref b in
			try
			(List.iter (fun ele -> u:= (min !u (ab_minmax (p-1) ele j (a, !u))); if !u<=a then raise (Return a)) nexte);
			!u
			with |Return(t) ->t
			end
		end

let joue (e: etat) (d: int) : etat =
	let rec aux (maxi, value) liste =
		match liste with
		|[] -> maxi
		|t::q -> let newValue = (ab_minmax d t e.joueur (min_int, max_int)) in
			if (newValue > value) then (aux (t, newValue) q) else (aux (maxi, value) q)
	in
	let etatListe = (next e) in
	(aux (List.hd etatListe, ab_minmax d (List.hd etatListe) e.joueur (min_int, max_int)) (next e))

let joue_lent (e: etat) (d): etat =
	let rec aux (maxi, value) liste =
		match liste with
		|[] -> maxi
		|t::q -> let newValue = (minmax d t e.joueur) in
			if (newValue > value) then (aux (t, newValue) q) else (aux (maxi, value) q)
	in
	let etatListe = (next e) in
	(aux (List.hd etatListe, minmax d (List.hd etatListe) e.joueur) (next e))


(* attend que le joueur clique sur l'interface graphique et retourne l'état du jeu après *)
let rec read_player_move (e: etat) =
  let st = Graphics.wait_next_event [Button_down] in
  let i = (st.mouse_x / cx) in
  match place_jeton e i with
  | None -> read_player_move e
  | Some(g') -> g'

let main () =
  Graphics.open_graph (" " ^ (string_of_int sx) ^ "x" ^ (string_of_int sy));
  (* définition du jeu initial *)
  let init = { game = Array.make_matrix nb_colonnes nb_lignes None ; joueur = Rouge } in
  let rec play_with_user (e: etat) =
    Graphics.clear_graph ();
    draw_game e;
    (* le joueur joue *)
    let e' = read_player_move e in
    Graphics.clear_graph ();
    draw_game e';
    (* l'ordinateur joue *)
    let e'' = joue e' 7 in
    play_with_user e''
  in
  play_with_user init
