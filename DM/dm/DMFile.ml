type couleur = Blanc |Noir;;
type quadTree = Feuille of couleur |Noeud of quadTree*quadTree*quadTree*quadTree;;

type bit_matrice = int list list;;
type str_matrice = string list list;;

type bit = Zero |Un;;

type matrice = string array array

exception Invalid_coordinates of int*int;;

(* QUESTION 1 ------------------------------------------- *)

let rec quadTree_full n = 
  if (mod) n 2=0 then
    match n with
    |2 -> Noeud(Feuille Noir, 
                Feuille Noir, 
                Feuille Noir, 
                Feuille Noir)
    |a -> Noeud(quadTree_full (n/2), 
                quadTree_full (n/2), 
                quadTree_full (n/2), 
                quadTree_full (n/2))
  else raise (Invalid_argument "taille invalide");;

let rec quadTree_empty n = 
  if (mod) n 2=0 then
    match n with
    |2 -> Noeud(Feuille Blanc, 
                Feuille Blanc, 
                Feuille Blanc, 
                Feuille Blanc)
    |a -> Noeud(quadTree_empty (n/2), 
                quadTree_empty (n/2), 
                quadTree_empty (n/2), 
                quadTree_empty (n/2))
  else raise (Invalid_argument "taille invalide");;

(* QUESTION 2 ------------------------------------------- *)  

let rec inverse tree = 
  match tree with
  |Feuille Noir -> Feuille Blanc
  |Feuille Blanc -> Feuille Noir 
  |Noeud (n1,n2,n3,n4) -> Noeud (inverse n1,inverse n2,inverse n3,inverse n4);;

(* QUESTION 3 ------------------------------------------- *)

let rec rotate a = 
  match a with
  |Feuille _ -> a
  |Noeud (n1,n2,n3,n4) -> Noeud (rotate n2,rotate n3, rotate n4, rotate n1);;

(* QUESTION 4 ------------------------------------------- *)

let rec union a b = 
  match a,b with 
  |Feuille Blanc,Feuille Noir -> Feuille Noir  
  |Feuille Noir,Feuille Blanc -> Feuille Noir  
  |Feuille Noir,Feuille Noir -> Feuille Noir  
  |Feuille Blanc,Feuille Blanc -> Feuille Blanc
  |Noeud (a1,a2,a3,a4),Noeud (b1,b2,b3,b4) 
      -> Noeud (union a1 b1, union a2 b2, union a3 b3, union a4 b4)

  (* même taille donc "inutile" tree pas opti *)
  |Feuille Blanc,Noeud (_,_,_,_) -> failwith "tailles différentes"
  |Noeud (_,_,_,_),Feuille Blanc -> failwith "tailles différentes"
  |Noeud (_,_,_,_),Feuille Noir  -> failwith "tailles différentes"
  |Feuille Noir,Noeud (_,_,_,_) -> failwith "tailles différentes";;


(* QUESTION 5 ------------------------------------------- *)

let rec intersection a b = 
  match a,b with 
  |Feuille Blanc,Feuille Noir -> Feuille Blanc
  |Feuille Noir,Feuille Blanc -> Feuille Blanc
  |Feuille Blanc,Feuille Blanc -> Feuille Blanc
  |Feuille Noir,Feuille Noir -> Feuille Noir  
  |Noeud (a1,a2,a3,a4),Noeud (b1,b2,b3,b4) 
      -> Noeud (intersection a1 b1, intersection a2 b2, intersection a3 b3, intersection a4 b4)

  (* même taille donc "inutile" tree pas opti *) 
  |Feuille Blanc,Noeud (_,_,_,_)  -> failwith "tailles différentes"
  |Noeud (_,_,_,_),Feuille Blanc -> failwith "tailles différentes"
  |Noeud (_,_,_,_),Feuille Noir  -> failwith "tailles différentes"
  |Feuille Noir,Noeud (_,_,_,_) -> failwith "tailles différentes";;


(* QUESTION 6 ------------------------------------------- *)

let rec color (x,y) tree taille_i =
  match tree with
  |Feuille c -> c
  |Noeud (n1, n2, n3, n4) 
      -> let k = (taille_i/2) 
      in 
          match (x/k, y/k) with
          |(0, 0) -> color (x, y) n1 k
          |(1, 0) -> color (x-k, y) n4 k
          |(0, 1) -> color (x, y-k) n2 k
          |(1, 1) -> color (x-k, y-k) n3 k
          |_ -> raise (Invalid_coordinates (x,y));;


(* QUESTION 7 ------------------------------------------- *)

let modify tree taille_i p =
  let rec aux x y tree taille =
    match tree with 
    |Feuille _ -> Feuille (p x y (color (x,y) tree taille_i))
    |Noeud (n1, n2, n3, n4) 
    -> let k = taille/2 in  
      Noeud(aux x y n1 k,
            aux (x) (y+k) n2 k, 
            aux (x+k) (y+k) n3 k, 
            aux (x+k) (y) n4 k)
  in aux 0 0 tree taille_i;;


(* QUESTION 8 ------------------------------------------- *)

let rec optimise tree =
  match tree with 
      |Feuille _ -> tree
      |Noeud (Feuille c1, Feuille c2, Feuille c3, Feuille c4) 
        when c1=c2 && c2=c3 && c3=c4 -> Feuille c1
      |Noeud (n1,n2,n3,n4) -> Noeud (optimise n1,optimise n2,optimise n3,optimise n4);;


(* QUESTION 9 ------------------------------------------- *)

let rec quadtree_to_list tree = 
    match tree with
    |Feuille Blanc -> [Zero ; Zero]
    |Feuille Noir  -> [Zero ; Un ]
    |Noeud (n1,n2,n3,n4) 
    ->  Un::quadtree_to_list n1 
        @ quadtree_to_list n2 
        @ quadtree_to_list n3 
        @ quadtree_to_list n4;;


(* QUESTION 10 ------------------------------------------- *)

let rec list_to_quadtree l = 
  match l with
  |Zero::Zero::r -> Feuille Blanc, r
  |Zero::Un::r -> Feuille Noir, r
  |Un::r ->
      let a1,r = list_to_quadtree r in
      let a2,r = list_to_quadtree r in
      let a3,r = list_to_quadtree r in
      let a4,r = list_to_quadtree r in
          Noeud (a1,a2,a3,a4),r
  (* pas obligé mais sinon matching pattern non exhaustif *)
  |_ -> assert false;; 

let end_list_to_quadtree l = let a,_ = list_to_quadtree l in a;;


(* TESTS SECTION - IMPLEMENTATIONS ----------------------- *)

let n = 8;; (* taille choisie *)

(* 1 *) 
let tree = quadTree_full n;;
let full = quadTree_full n;;
let empty = quadTree_empty n;;

(* utilisée pour montrer que rotate() fonctionne *)
let modified_tree = modify tree n (fun x y c -> if x<(n/2) && y<(n/2) && c=Noir then Blanc else Noir);;

(* 2 *)
let rotate_matrix = rotate modified_tree;; 

(* 3 *)
let inverse_matrix = inverse rotate_matrix;;

(* 4 *)
let union = union rotate_matrix inverse_matrix;;

(* 5 *)
let intersection = intersection full empty;;

(* 6 COLOR OK *)
let couleur_feuille = color (3,3) modified_tree n;; 

(* 7 *)
let modified_tree = modified_tree;; 

(* 8 *)
let optimise = optimise (quadTree_full n);; 

(* Ne seront pas print dans le terminal *)

(* 9 *)
let bits_liste = quadtree_to_list modified_tree;; 

(* 10 *)
let bits_to_quadtree = end_list_to_quadtree (quadtree_to_list (quadTree_empty n));;


(* SECTION CONSIDEREE COMME BONUS *)

(* Questions BONUES sont utilisées pour print et dans le fichier main.ml *)

(* QUESTIONS BONUS --------------------------------------- *)

(* Code utilisé dans la section OUTPUT, mit en commentaire pour ne pas altérer la suite *)
(*
let n = 8;;
let tree = quadTree_empty (n);;
let init f = Array.init n (fun i -> Array.init n (f i));;
let tree_matrix = (init (fun i j 
    -> if (color (i,j) tree n)=Noir then "n" else "b") : matrice);;
*)
  
(* VERSION AVEC GRAPHICS => doit être installé si os = osx -version : <= 10.12 *)
(*
open Graphics
type image = couleur array array;;

let taille = 8

(* après modification du tree *)
let tree = modify tree taille (fun x y c -> if c=Blanc && (x,y)<(n-1,n-1) then Noir else Blanc);;
let tree_matrix = (init (fun i j 
    -> if (color (i,j) tree taille)=Noir then "n" else "b") : matrice);;

let img = (init (fun i j 
    -> if (color (i,j) tree taille)=Noir then Blanc else Noir) : image);;
    
let rec img_to_tree img i j k =
    if k <= 1 then Feuille img.(i).(j)
    else
        let k' = k/2 in
        let c1 = img_to_tree img i j k'
        and c2 = img_to_tree img i (j+k') k'
        and c3 = img_to_tree img (i+k') j k'
        and c4 = img_to_tree img (i+k') (j+k') k'
        in
        match c1,c2,c3,c4 with
        |Feuille n1, Feuille n2, Feuille n3, Feuille n4
          (* opti: if same children of c => node => feuille c*)
          when n1 = n2 && n2 = n3 && n3 = n4 -> c1 
        |_,_,_,_ ->  Noeud (c1,c2,c3,c4);;

let image_vers_arbre k img = img_to_tree img 0 0 k;;
let tree = image_vers_arbre (Array.length img) img;;

let rec output_img i j k tree = 
    match tree with
    |Feuille Noir 
        (* on veut output que les coords noires *)
        ->  Graphics.fill_rect i j k k 
    |Feuille Blanc 
        -> ()
    |Noeud (c1,c2,c3,c4) 
        ->
        let k' = k/2 in
        output_img i (j+k') k' c2 ;
        output_img (i+k') (j+k') k' c3 ;
        output_img i j k' c1 ;
        output_img (i+k') j k' c4;;
        
let dessine_quadtree taille tree = output_img 0 0 k tree;;
*)


(* OUTPUT SECTION ------------------------------------------- *)

(* Commande : ocaml DMFile.ml et FAIRE ATTENTION : après modifications les tailles peuvent varier *)

let init f n = List.init n (fun i -> List.init n (f i));;

let rec print_bits_matrix l =   
  match l with 
  [] -> () ; print_string "\n"
  |e::l -> print_int e ; print_string " " ; print_bits_matrix l;;
  
let rec print_str_matrix l =   
  match l with 
  [] -> () ; print_string "\n"
  |e::l -> print_string e ; print_string " " ; print_str_matrix l;;

(* output 2 types de matrices : int (bits) OU str (string) *)
let outputBitsMat m = List.iter (fun x -> print_bits_matrix x) m;;
let outputStrMat m = List.iter (fun x -> print_str_matrix x) m;;


(* Passer le tree à output dans bit_matrix et str_matrix *)

let output = intersection;;

let bit_matrix = (init (fun i j 
  -> if (color (i,j) output n)=Noir then 1 else 0) n : bit_matrice);;   

let str_matrix = (init (fun i j 
    -> if (color (i,j) output n)=Noir then "n" else "b") n : str_matrice);;
  
print_string "\n";
print_string "Bits matrix : \n";
outputBitsMat (bit_matrix);;

print_string "\n";
print_string "Strings/chars matrix : \n";
outputStrMat (str_matrix);;

print_string "\n";
print_newline()
