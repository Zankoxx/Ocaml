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

let f1 = Or ( And ( And ( Equ ( Imp ( Symb "e2" ,
                                      And ( Top , Symb "e1") ) ,
                                And ( Bot , Not ( Symb "e1" ) ) ) ,
                          Symb "e4" ),
                    ( Not ( Symb "e3" ) ) ) ,
              Imp ( Symb "e1" , Imp ( Symb "e1" , Symb "e2" ) ) ) ;;

let f2 = Imp(Symb "p",Symb "p");;
let f2bis = Or(Symb "p" ,Not(Symb "p"));;
let f3 = And(Symb "p" ,Not(Symb "p"));;

let f4 = And(Imp(Symb "p",Symb "p"),Imp(Symb "r",Symb "r"));;
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
let i1 = [("a",Un);("b",Zero);("c",Un)];;
let i2 = [("a",Zero);("b",Zero);("c",Zero)];;
let i3 = [("a",Un);("b",Un);("c",Un)];;

let listeInterpretation = [
  [("p",Zero);("q",Zero)];
  [("p",Zero);("q",Un)];
  [("p",Un);("q",Zero)];
  [("p",Un);("q",Un)];
];;

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

let modele : prop * interpretation -> bool = function
  | (Top, _) -> true
  | (Bot, _) -> false
  | (Symb s, i) -> valeur (Symb s, i) = Un
  | (Not p, i) -> valeur (Not p, i) = Un
  | (And(p1, p2), i) -> valeur (And(p1, p2), i) = Un
  | (Or(p1, p2), i) -> valeur (Or(p1, p2), i) = Un
  | (Imp(p1, p2), i) -> valeur (Imp(p1, p2), i) = Un
  | (Equ(p1, p2), i) -> valeur (Equ(p1, p2), i) = Un;;

      
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

let rec existeModele : prop * interpretation list -> bool = function (p,i) ->
  match p,i with 
  |(_,[]) -> false 
  |(p,t::q) -> if modele(p,t) then true else existeModele(p,q);;

let rec tousModele : prop * interpretation list -> bool = function (p,i) ->
  match p,i with 
  |(p,t::q) -> if modele(p,t) then existeModele(p,q) else false
  |(_,[]) -> true;;

let rec memeModele : prop * prop * interpretation list -> bool = function (p1,p2,i) ->
  match p1,p2,i with
  |(p1,p2,t::q) -> if modele(p1,t) = modele(p2,t) then memeModele(p1, p2, q) else false 
  |(_,_,[]) -> true;;

let satisfiable : prop -> bool = function p ->
  existeModele(p,(ensInt (sp p)));;

let insatisfiable : prop -> bool = function p -> 
  satisfiable p = false ;;

let valide : prop -> bool = function p -> tousModele(p,(ensInt(sp p)));;

let equivalent1 : prop * prop -> bool = function (p1,p2) -> 
  memeModele(p1,p2, (ensInt ((sp p1)@(sp p2)))) ;;

let equivalent2 : prop * prop -> bool = function (p1,p2) -> 
  valide(Equ (p1,p2));;

let ExisteUnContreExempleConsequenceLogique : prop * prop * interpretation list -> bool = function (h,c,EI) match EI with
    |[] -> false
    |t::q when modele(h,t) && not(modele(c,t)) -> true 
    |_::q -> ExisteUnContreExempleConsequenceLogique(h,c,q) ;;

let consequence : prop * prop -> bool = function (h,c) -> 
  not(ExisteUnContreExempleConsequenceLogique(h,c,(ensInt(union(sp h),(sp c)))));;
  

let rec tousSP : prop list -> string list = function p match p with
    |[] -> []
    |t::q -> union(sp t,tousSP q);;

let rec modeleCommun : prop list * interpretation -> bool = function (p,i) -> match p with
  |[] -> true 
  |t::q -> if modele(t,i) then modeleCommun(q,i) else false;;



let rec existeModeleCommun : prop list * 

;;
let contradictoire : prop list -> bool = function
  | lp -> not (existeModeleCommun lp)
  | 


  
  
  
  
  





  
  
  
  
  
  





  
          

            
        
     
      
      
      
      
      
      
      
      
      
      
      
      
      
    (*     
      let inteval = [("e1",Zero);("e2",Zero);("e3",Zero);("e4",Zero)];;
(*** test Q3 ***)
      prof fbfeval;;
(*** test Q4 ***)
      sp fbfeval;;
(*** test Q5 ***)
      affiche fbfeval ;;
(*** test Q6 ***)
(*affichePri fbfeval ;;*)
(*** test Q10 ***)
      valV fbfeval inteval;;
(*** test Q14 ***)
      satisfiable fbfeval;;
(*** test Q15 ***)
      valide fbfeval ;;
(*** test Q17 ***)
      equivalent1 (Symb "b") (And (Top, Symb "a"));;
      equivalent2 (And (Symb "b",Not (Symb "b"))) (Not (Imp (Symb "a", Symb "a")));;
(*** test Q18 ***)
      consequence2 (Not fbfeval) (And (Symb "e1", Not (Symb "e2")));;
(*** test Q22 ***)
      consequence [Top ; Bot] Top ;;
(*** test Q23 ***)
      consequenceV [fbfeval ; (Not (Or (Symb "e1",Symb "e2")))] (Imp (Symb "e4", Symb "e3"));;
(*** test Q24 ***)
      consequenceI [fbfeval ; (Not (Or (Symb "e1",Symb "e2"))) ; (And (Symb "e4",
                                                                       Not (Symb "e3"))) ] (Equ (Symb "e1", Symb "e3"));;;;
      
      
      
   *)   
      
      
      
      
      
                  
