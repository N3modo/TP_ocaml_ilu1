let rec insertObj elemO listeCouple = match listeCouple with
  |[] -> (1,elemO) :: []
  |tete :: queue -> match tete with (a,b) ->
    if b=elemO then (a+1,b) :: queue 
    else tete :: (insertObj elemO queue)
                 
let rec insert indexP elemO listeCouple = match listeCouple with
  |[] -> (indexP,[1,elemO]) :: []
  |tete :: queue -> match tete with a,b -> 
    if a=indexP then (a,(insertObj elemO b)) :: queue 
    else tete :: insert indexP elemO queue 
           
let rec decompte listeCouple = match listeCouple with
  |[] -> []
  |(x,y) :: queue -> insert x y (decompte queue)
                       
let rec longueur listeCouple = match listeCouple with
  |[] -> 0
  |(x,y) :: queue -> x + longueur queue
                       
let rec aumoins_indexed n listeCouple = match listeCouple with
  |[]->[]
  |(x,y) :: queue -> if (longueur y) >= n then x :: (aumoins_indexed n queue)
      else aumoins_indexed n queue
          
let aumoins n listeCouple = aumoins_indexed n (decompte listeCouple)