(*
** RecuitSimule
** Copyright 2006 Quentin Quadrat <lecrapouille@gmail.com>
**
** This file is part of RecuitSimule.
**
** RecuitSimule is free software: you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation, either version 3 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful, but
** WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
** General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
*)

(*
** RecuitSimule.ml for Recuit Simule
**
** Made by Quentin Quadrat
** Mail   <lecrapouille@gmail.com>
**
** Started on  Tue Oct  3 01:47:34 2006 Quentin Quadrat
** Last update Mon Oct 16 14:41:38 2006 Quentin Quadrat
*)

#load "graphics.cma";;

(* circuit de taille 3 x 3 composants *)
let taille = 5 ;;

(* List de composants de 1 a 9 (ID, position, [voisin 1; voisin 2; ...]) *)
let circuit =
  [
    (1, ref 1, [2;6]);
    (2, ref 2, [1;3;7]);
    (3, ref 3, [2;4;8]);
    (4, ref 4, [3;9;5]);
    (5, ref 5, [4;10]);
    (6, ref 6, [1;7;11]);
    (7, ref 7, [2;6;8;12]);
    (8, ref 8, [3;7;9;13]);
    (9, ref 9, [4;8;10;14]);
    (10, ref 10, [5;9;15]);
    (11, ref 11, [6;12;16]);
    (12, ref 12, [7;11;13;17]);
    (13, ref 13, [8;12;14;18]);
    (14, ref 14, [9;13;15;19]);
    (15, ref 15, [10;14;20]);
    (16, ref 16, [11;17;21]);
    (17, ref 17, [12;16;18;22]);
    (18, ref 18, [13;17;19;23]);
    (19, ref 19, [14;18;20;24]);
    (20, ref 20, [15;19;25]);
    (21, ref 21, [16;22]);
    (22, ref 22, [17;21;23]);
    (23, ref 23, [18;22;24]);
    (24, ref 24, [19;23;25]);
    (25, ref 25, [20;24])
  ]
;;

(* Accesseur n-ieme composant *)
let get_composant circuit n = List.nth circuit (n-1) ;;

(* modifie la nouvelle position du composant n *)
let set_position circuit n posi =
  let (_,p,_) = get_composant circuit n in p := posi;;

(* Accesseur position du composant n *)
let get_position circuit n =
  let (_,p,_) = get_composant circuit n in !p ;;

(* Donne les coordonnees (X,Y) en fonction du numero de position 'n' *)
(* List.map position [1;2;3;4;5;6;7;8;9] ;; *)
let posi2coord n = (1 + (n - 1) mod taille, 1 + (n - 1) / taille) ;;

(* Donne la longueur Manhattan (en L) entre 2 composants *)
let long_manhattan circuit c1 c2 =
  let (x1,y1) = posi2coord (get_position circuit c1) in
  let (x2,y2) = posi2coord (get_position circuit c2) in
    abs (x1 - x2) + abs (y1 - y2)
;;

(* Calcule l'energie locale d'un composant n *)
(* calcul_energie 4;; *)
let energie_locale circuit n =
  let (_,_,voisins) = get_composant circuit n in
  let liste = List.map (long_manhattan circuit n) voisins in
    List.fold_left (+) 0 liste
;;

(* Energie totale du circuit *)
let energie_totale circuit =
  let l = List.map (fun (x,_,_) -> energie_locale circuit x) circuit in
  let res = List.fold_left (+) 0 l in
(*    print_string "Nouvelle energie = ";
    print_int res;
    print_char '\n';*)
    res
;;

Graphics.open_graph "";;

let dessine_arc circuit c1 c2 =
  let (x1,y1) = posi2coord (get_position circuit c1) in
  let (x2,y2) = posi2coord (get_position circuit c2) in
    Graphics.moveto (x1 * 75 + Graphics.size_x () / 10) (y1 * 75 + Graphics.size_y () / 10);
    Graphics.lineto (x2 * 75 + Graphics.size_x () / 10) (y2 * 75 + Graphics.size_y () / 10)
;;

let dessine_noeud circuit c =
  let (x,y) = posi2coord (get_position circuit c) in
    Graphics.set_color Graphics.background;
    Graphics.fill_circle (x * 75 + Graphics.size_x () / 10) (y * 75 + Graphics.size_y () / 10) 10;
    Graphics.set_color Graphics.foreground;
    Graphics.draw_circle (x * 75 + Graphics.size_x () / 10) (y * 75 + Graphics.size_y () / 10) 10;
    Graphics.moveto (x * 75 + Graphics.size_x () / 10) (y * 75 + Graphics.size_y () / 10 - 5);
    Graphics.draw_char (char_of_int (int_of_char 'A' + c))
;;

let dessine_circuit circuit =
  Graphics.clear_graph ();
  List.iter (fun (id,_,v) -> List.iter (dessine_arc circuit id) v) circuit;
  List.iter (fun (id,_,v) -> dessine_noeud circuit id) circuit
;;

(* dessine_circuit circuit;; *)

Random.self_init ();;
(* Random.init 15698;; *)

let commute circuit =
  let c1 = Random.int (taille * taille) + 1 in
(*    print_string "j'ai pris le numero ";
    print_int c1; *)
  let p1 = get_position circuit c1 in
  let c2 = Random.int (taille * taille) + 1 in
(*    print_string " et le numero ";
    print_int c2;
    print_char '\n'; *)
  let p2 = get_position circuit c2
  in
    set_position circuit c1 p2;
    set_position circuit c2 p1;
    (c1,p1,c2,p2)
;;

(* nouvelle nrj, ancienne nrj, temp actuelle *)
(* retourne (nb permutations acceptee, nb permutations tentees, nouvelle energie) *)
let acceptation circuit temp energie pa pt =
  let (c1,p1,c2,p2) = commute circuit in
  let en = float_of_int (energie_totale circuit) in
  let de = en -. energie in
    match de <= 0.0 with
      | true  -> (pa + 1, pt, en);
      | false ->
	  let p = exp (-1.0 *. de /. temp) in
	  let r = Random.float 1.0 in
(*	    print_string "Proba = "; print_float p; print_char '\n';
	    print_string "Rand  = "; print_float r; print_char '\n'; *)
	    match r <= p with
	      | true  -> (pa + 1, pt, en);
	      | false ->
		  set_position circuit c1 p1;
		  set_position circuit c2 p2;
		  (pa, pt + 1, energie)
;;

let equilibre_thermo pa pt =
  let max_pa = 12 * taille * taille in
  let max_pt = 100 * taille * taille in
    (pa >= max_pa) || (pt >= max_pt)
;;

let syst_fige iter nrj =
  let max_iter = 10000 in
  let min_energie = 0.001 in
    (iter >= max_iter) || (nrj <= min_energie)
;;

(* algo principal *)
let placement_de_composants circuit =
  let rec recuit_simule circuit temperature energie iter pa pt =
   (* print_string "\nIter="; print_int iter;
    print_string "\nT="; print_float temperature;
    print_string "\nE="; print_float energie; print_char '\n';*)
    let (ppa,ppt,nrj)  = acceptation circuit temperature energie pa pt in
      match equilibre_thermo ppa ppt with
	| false -> recuit_simule circuit temperature nrj (iter + 1) ppa ppt
	| true  -> (match syst_fige iter nrj with
		      | true  -> circuit(* Fin algo *)
		      | false -> recuit_simule circuit (temperature *. 0.9) nrj (iter + 1) ppa ppt)
  in
    recuit_simule circuit 666666.0 (float_of_int (energie_totale circuit)) 0 0 0
;;

commute circuit;;
commute circuit;;
commute circuit;;commute circuit;;
commute circuit;;
commute circuit;;
commute circuit;;
commute circuit;;
commute circuit;;
commute circuit;;
commute circuit;;
commute circuit;;

dessine_circuit circuit;;

dessine_circuit (placement_de_composants circuit);;


