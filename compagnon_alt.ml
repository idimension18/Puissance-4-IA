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
    Graphics.moveto (i * cx) 0;
    Graphics.lineto (i * cx) sy;
  done;
  for j = 0 to nb_lignes do
    Graphics.moveto 0  (j * cy);
    Graphics.lineto sx (j * cy);
  done;
  for i = 0 to (nb_lignes-1) do
    for j = 0 to (nb_colonnes-1) do
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

(* fonctions à compléter, elles sont définies ici pour pouvoir faire typer le reste du programme *)
let place_jeton (e: etat) (i: int): etat option =
  assert false

let joue (e: etat) (d: int): etat =
  assert false

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
  let init = { game = Array.make_matrix nb_lignes nb_colonnes None ; joueur = Rouge } in
  let rec play_with_user (e: etat) =
    Graphics.clear_graph ();
    draw_game e;
    (* le joueur joue *)
    let e' = read_player_move e in
    Graphics.clear_graph ();
    draw_game e';
    (* l'ordinateur joue *)
    let e'' = joue e' 5 in
    play_with_user e''
  in
  play_with_user init
