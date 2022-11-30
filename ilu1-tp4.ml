let rec hanoi (source, temp, dest) n = 
  if n<=0 then []
  else (hanoi (source, dest, temp) (n-1)) @ 
       (source,dest) :: 
       (hanoi (temp, source, dest) (n-1))
    (* 2^n *)

let rec map f l = match l with
  | [] -> []
  | tete :: queue -> f tete :: map f queue
                
let rec inserer valeur liste = match liste with
  |[] -> valeur :: []
  |tete :: queue -> 
      if valeur < tete then 
        valeur :: liste
      else tete :: inserer valeur queue 
             
let rec triInsertion l = match l with
  |[] -> []
  |tete :: queue -> inserer tete (triInsertion queue)
                      
let rec partage l = match l with
  |[] -> ([], [])
  |[a] -> ([a], [])
  |tete::tete2::queue -> let (a, b) = (partage queue) in (tete::a, tete2::b) 
                                                         
let rec merge l1 l2  = match l1,l2 with
  |[], _ -> l2
  |_, [] -> l1
  |(tete::queue, tete2::queue2) -> if tete<tete2 then 
        tete::(merge queue (tete2::queue2)) else 
        tete2::(merge (tete::queue) queue2)
               
let rec triFusion liste = match liste with 
  |[] | [_] -> liste
  |_ -> let (l1,l2) = partage liste in 
      merge (triFusion l1) (triFusion l2)
        
let fst (a,b) = a
  
let snd (a,b) = b
  
let rec listeP liste = match liste with
  | [] -> []
  | t :: queue -> (fst t) :: listeP queue

let rec estUnique liste a = match liste with 
  | [] -> true
  | tete :: queue -> (a <> tete ) && estUnique queue a

let estFonction liste = 
  let rec aux l1 = match l1 with
    | [] -> true
    | tete :: queue -> (estUnique queue tete) && (aux queue)
  in
  aux (listeP liste)

let rec image elem liste = match liste with 
  |[]->failwith("liste vide")
  |(a,b) :: queue -> if a = elem then b
      else image elem queue
          
let rec imageEns liste listeC = match liste with 
  |[] -> []
  |tete :: queue -> image tete listeC :: imageEns queue listeC 

let rec estInjective f = 
  let rec pasDoublon elem liste = match liste with
    |[] -> true
    |(a,b) :: queue -> b <> elem && pasDoublon elem queue
  in
  match f with 
  |[] -> true
  |(a,b) :: queue -> pasDoublon b queue && estInjective queue
                       
let rec surcharge f1 f2 = 
  let rec pasDoublon elem liste = match liste with
    |[] -> true
    |(a,b) :: queue -> a <> elem && pasDoublon elem queue
  in
  match f1,f2 with 
  |[],_ -> f2
  |_,[] -> f1
  |(f1a,f1b) :: queuef1, (f2a,f2b) :: queuef2 -> 
      if pasDoublon f1a f2
      then (f1a,f1b) :: surcharge queuef1 f2
      else (f2a,f2b) :: surcharge queuef1 queuef2
             
let rec isDef x f = 
  match f with 
  | [] -> false
  | (a,b) :: queue -> a = x || isDef x queue             
                        
let rec composition f1 f2 = 
  match f2 with
  | [] -> []
  | (a,b) :: queue -> if isDef b f1 then (a,image b f1) :: composition f1 queue
      else composition f1 queue
  
let rec produit f1 f2 =
  let rec sousProduit x y f = 
  match f with
  |[]->[]
  |(x1,y1) :: queue1 -> ((x,x1),(y,y1)) :: sousProduit x y queue
  in
  match f1 with
  |[] -> []
  |(x,y) :: queue -> sousProduit x y f2 @ produit queue f2
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
