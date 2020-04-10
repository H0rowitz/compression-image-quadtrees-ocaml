type couleur = Blanc |Noir;;
type quadTree = Feuille of couleur |Noeud of quadTree*quadTree*quadTree*quadTree;;

type bit_matrice = int list list;;
type str_matrice = string list list;;

exception Invalid_coordinates of int*int;;

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
  else raise (Invalid_argument "argument invalide");;

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
    else raise (Invalid_argument "argument invalide");;

(* anti-clockwise *)
let rec rotate a = 
  match a with
  |Feuille _ -> a
  |Noeud (n1,n2,n3,n4) -> Noeud (rotate n2,rotate n3, rotate n4, rotate n1);;

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
                

(* FONCTIONS UTILES A L'OUTPUT DANS TERMINAL QUI ONT ETE ADD *)
(* REMARQUE : si coordonnées incorrectes => ne va pas lever d'exception, va juste process sans modification *)

let init f n = List.init n (fun i -> List.init n (f i));;

let rec print_bits_matrix l =   
  match l with 
  [] -> () ; print_string "\n"
  |e::l -> print_int e ; print_string " " ; print_bits_matrix l;;
  
let rec print_str_matrix l =   
  match l with 
  [] -> () ; print_string "\n"
  |e::l -> print_string e ; print_string " " ; print_str_matrix l;;
    
let outputBitsMat m = List.iter (fun x -> print_bits_matrix x) m;;
let outputStrMat m = List.iter (fun x -> print_str_matrix x) m;;

let to_color_type s = if s="Noir" then Noir else Blanc;;

let not_color c =
  match c with 
  |Blanc -> Noir 
  |Noir -> Blanc;;

let main () =

  if (Array.length Sys.argv) = 0 then begin 
    print_string "Entrer la commande : ./file info"; 
    print_newline()   

  end else begin
      try 

        if Sys.argv.(1) = "tree" then 
    
          let tree = 
            if Sys.argv.(2) = "full" then 
              quadTree_full (int_of_string Sys.argv.(3)) 
           
            else if Sys.argv.(2) = "empty" then 
              quadTree_empty (int_of_string Sys.argv.(3))

            else 
              (* sinon par défaut on assigne un tree *)
              quadTree_empty (int_of_string Sys.argv.(3))
            
          and n = int_of_string Sys.argv.(3) 
          and coord1 = int_of_string Sys.argv.(4)
          and coord2 = int_of_string Sys.argv.(5)
          and tree_color = to_color_type Sys.argv.(6)
          in  
            let tree_matrix = 
                modify tree n (fun x y c -> if c=tree_color && x<coord1 && y<coord2 then c else not_color c) 
            in 
              let last_tree = if (Sys.argv.(7) = "true") then rotate tree_matrix else tree_matrix
              in 
                let bit_matrix = (init (fun i j 
                  -> if (color (i,j) last_tree n)=Noir then 1 else 0) n : bit_matrice)
                in 
                  outputBitsMat (bit_matrix)

        else if Sys.argv.(1) = "info" then 

          print_string "
          Structure de l'input : avec préfix ./name_compiled_file:

          *) tree <type> <taille> <x> <y> <color> <rotate:false>
            - x et y : juste pour tester modify
            - rotate : false or true 
             
          *) ex d'usage : ./name_compiled_file tree full 16 8 8 Noir true ";
          print_newline ();
          print_newline ()

      with 
        |Invalid_argument _  -> raise (Invalid_argument "entrer un arg valide.\n")
        |Invalid_coordinates (a,b) -> raise (Invalid_coordinates (a,b))
        |_ -> failwith "Fonction inexistante ou oubli params,\nEntrez la commande : ./file info\n"
      
  end;;

if !Sys.interactive then () else main ();;