type couleur = Blanc | Noir
type quadTree =
    Feuille of couleur
  | Noeud of quadTree * quadTree * quadTree * quadTree
type bit_matrice = int list list
type str_matrice = string list list
type bit = Zero | Un
type matrice = string array array
exception Invalid_coordinates of int * int
val quadTree_full : int -> quadTree
val quadTree_empty : int -> quadTree
val inverse : quadTree -> quadTree
val rotate : quadTree -> quadTree
val color : int * int -> quadTree -> int -> couleur
val modify :
  quadTree -> int -> (int -> int -> couleur -> couleur) -> quadTree
val quadtree_to_list : quadTree -> bit list
val list_to_quadtree : bit list -> quadTree * bit list
val end_list_to_quadtree : bit list -> quadTree
val full : quadTree
val empty : quadTree
val rotate_matrix : quadTree
val inverse_matrix : quadTree
val union : quadTree
val intersection : quadTree
val couleur_feuille : couleur
val modified_tree : quadTree
val optimise : quadTree
val bits_liste : bit list
val bits_to_quadtree : quadTree
val n : int
val tree : quadTree
val tree_matrix : matrice
val init : (int -> int -> 'a) -> int -> 'a list list
val print_bits_matrix : int list -> unit
val print_str_matrix : string list -> unit
val outputBitsMat : int list list -> unit
val outputStrMat : string list list -> unit
val output : quadTree
val bit_matrix : bit_matrice
val str_matrix : str_matrice
