(* Ceci est un éditeur pour OCaml
   Entrez votre programme ici, et envoyez-le au toplevel en utilisant le
   bouton "Évaluer le code" ci-dessous ou [Ctrl-e]. *)
   
  
    
(* ===== Définition des types ===== *)
type valVerite = Zero | Un;;

type prop =
  | Symb of string
  | Top
  | Bot
  | Not of prop
  | And of prop * prop
  | Or of prop * prop
  | Imp of prop * prop
  | Equ of prop * prop;;

type interpretation = (string * valVerite) list;;

(* ===== Fonctions utilitaires sur les listes ===== *)
let rec estPresent: (string * string list) -> bool = function
    (s,l) -> if l = [] then false
      else if s = List.hd l then true
      else estPresent(s,List.tl l);;

let ajouteSiAbsent: (string * string list) -> string list = function
    (a,li) -> if estPresent(a,li) then li
      else a::li;;

let rec union: (string list * string list) -> string list = function
    (l1,l2) -> match l2 with
    | [] -> l1
    | e::l -> union(ajouteSiAbsent(e,l1),l);;

(* ===== Fonctions sémantiques ===== *)
let rec nbc: prop -> int = function
    p -> match p with
    | Symb _ -> 0
    | Top -> 0
    | Bot -> 0
    | Not p -> 1 + nbc p
    | And(p1,p2)
    | Or(p1,p2)
    | Imp(p1,p2)
    | Equ(p1,p2) -> 1 + nbc p1 + nbc p2;;

let rec prof: prop -> int = function
    p -> match p with
    | Symb _ -> 0
    | Top -> 0
    | Bot -> 0
    | Not p -> prof p + 1
    | And(p1,p2)
    | Or(p1,p2)
    | Imp(p1,p2)
    | Equ(p1,p2) -> 1 + max (prof p1) (prof p2);;


let rec sp: prop -> string list = function
    p -> match p with
    | Top -> []
    | Bot -> []
    | Symb s -> [s]
    | Not p -> sp p
    | And(p1,p2)
    | Or(p1,p2)
    | Imp(p1,p2)
    | Equ(p1,p2) -> union(sp p1, sp p2);;


(* ===== Fonction d’affichage ===== *)
let rec affiche: prop -> string = function
    p -> match p with
    | Symb a -> a
    | Top -> "T"
    | Bot -> "⊥"
    | Not p -> "("^" ¬ "^ affiche(p) ^")"
    | And(p1,p2) -> "("^ affiche(p1) ^" ∧ "^ affiche(p2) ^")"
    | Or(p1,p2) -> "("^ affiche(p1) ^" ∨ "^ affiche(p2) ^")"
    | Imp(p1,p2) -> "("^ affiche(p1) ^" → "^ affiche(p2) ^")"
    | Equ(p1,p2) -> "("^ affiche(p1) ^" ↔ "^ affiche(p2) ^")";;

(* ===== Interprétation ===== *)
let rec intSymb: string * interpretation -> valVerite = function
    (s,i) -> match (s,i) with
    | (s,[]) -> raise (Failure "Variable non interprétée")
    | (s,(symb,v)::q) -> if s = symb then v else intSymb(s,q);;

(* ======== Interpréation de constantes logiques,connecteur unaire,connecteur binaire ======== *)
let intTop: valVerite = Un;;       
let intBot: valVerite = Zero;;     

                                   
let intNot: valVerite -> valVerite = function
  | Un -> Zero
  | Zero -> Un;;


let intAnd: valVerite * valVerite -> valVerite = function
  | (Un, Un) -> Un
  | _ -> Zero;;

let intOr: valVerite * valVerite -> valVerite = function
  | (Zero, Zero) -> Zero
  | _ -> Un;;

let intImp: valVerite * valVerite -> valVerite = function
  | (Un, Zero) -> Zero
  | _ -> Un;;

let intEqu: valVerite * valVerite -> valVerite = function
  | (Un, Un) | (Zero, Zero) -> Un
  | _ -> Zero;;


let rec valeur: prop * interpretation -> valVerite = function
  | (p, i) -> match p with
    | Top -> intTop
    | Bot -> intBot
    | Symb s -> intSymb (s, i)
    | Not s -> intNot (valeur (s, i))
    | And(p1, p2) -> intAnd (valeur (p1, i), valeur (p2, i))
    | Or(p1, p2) -> intOr (valeur (p1, i), valeur (p2, i))
    | Imp(p1, p2) -> intImp (valeur (p1, i), valeur (p2, i))
    | Equ(p1, p2) -> intEqu (valeur (p1, i), valeur (p2, i))
;;


let i1 = [("a",Un);("b",Zero);("c",Un)];;
let i2 = [("a",Zero);("b",Zero);("c",Zero)];;
let i3 = [("a",Un);("b",Un);("c",Un)];;

let listeInterpretation = [
  [("p",Zero);("q",Zero)];
  [("p",Zero);("q",Un)];
  [("p",Un);("q",Zero)];
  [("p",Un);("q",Un)];
];;

let rec consTous: 'a * 'a list list -> 'a list list = function
    (e,idl) -> match idl with
    | [] -> []
    | t::q -> (e::t) :: consTous(e,q);;

let rec ensInt: string list -> (string * valVerite) list list = function
    ls -> match ls with
    | [] -> [[]]
    |s::q ->
        (consTous ((s, Zero), ensInt q))
        @
        (consTous ((s, Un) , ensInt q));;


            
        
     
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
                  