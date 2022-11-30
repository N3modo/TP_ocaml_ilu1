let estZero_v1 n =
  match n with
  | 0 -> "zero"

let estZero_v2 n =
  match n with
  | 0 -> "zero" 
  | _ -> "nonZero"

let voyelle a =
  match a with
  | 'a'
  | 'e'
  | 'i'
  | 'o'
  | 'u'
  | 'y' -> true
  | _ -> false
    
let rang x =
  match x with 
  | "lundi" -> 1
  | "mardi" -> 2
  | "mercredi" -> 3
  | "jeudi" -> 4
  | "vendredi" -> 5
  | "samedi" -> 6
  | "dimanche" -> 7
  | _ -> 0 
    
let inf j1 j2 = let a, b = (rang j1),(rang j2) in match a, b with
  | 0, _ 
  | _ , 0 -> false
  | 7, 1 -> true
  | _, _ -> (a = b-1)
                                   
let jsem x =
  match x with 
  |   1 ->"lundi"
  |  2 ->"mardi"
  |  3 ->"mercredi"
  |  4 ->"jeudi"
  |   5 ->"vendredi"
  |   6 ->"samedi"
  |   7 ->"dimanche"
  | _ -> "jour inconnu"
    
let jourSucc1 x =
  match x with 
  |   "dimanche" ->"lundi"
  |   "lundi"->"mardi"
  |   "mardi" ->"mercredi"
  |   "mercredi" ->"jeudi"
  |   "jeudi" ->"vendredi"
  |   "vendredi" ->"samedi"
  |   "samedi" ->"dimanche"
  | _ -> "jour inconnu"
    
let jourSucc2 x = jsem (
    let h = (rang x) in 
    if h = 0 then
      0 
    else 
      let j = h+1 in 
      if j = 8 then
        1 else 
        j)
    
let jourSucc3 x = let a = rang(x) in 
  jsem (if a<>0 then 
          ((a mod 7) +1) 
        else 
          0) 

let jourPred1 x =
  match x with 
  |   "lundi"->"dimanche" 
  |   "mardi"->"lundi"
  |   "mercredi"->"mardi"
  |    "jeudi"->"mercredi"
  |    "vendredi"->"jeudi"
  |    "samedi"->"vendredi"
  |    "dimanche"->"samedi"
  | _ -> "jour inconnu"
    
let jourPred2 x = jsem (let h = (rang x) in
                        if h = 0 then 0 
                        else let j = h-1 in
                          if j = 0 then 7 
                          else j)
    
let jourPred3 x = let a = rang(x) in
  jsem (if a = 1 then 7 
        else (((a-2) mod 7)+1))


let bissextile annee = (annee mod 4)=0 && (annee mod 100)<>0 || (annee mod 400)=0

let nbjour mois annee  = 
  match mois with
  |1 
  |3
  |5
  |7
  |8
  |10
  |12-> 31
  |2 -> if bissextile annee then 29 else 28
  |4 
  |6
  |9
  |11 -> 30
  |_ -> failwith "cketuve"























    