let multiple_of n d =
  n mod d = 0
            
let integer_square_root n =
  let g = float_of_int n in
  let temp = sqrt g in
  int_of_float temp
    
let last_character chaine = chaine.[String.length chaine-1]
  
let string_of_bool n =
  if n then "true" else "false" 
    
let pairwise_distinct (a,b,c,d) = a<>b && a<>c && a<>d && b<>c && b<>d && c<>d 
                                                                          
let e1 = ((2 , 1=1),1)
         
let e2 = ((fun x -> x), 'b')
         
let f1 a b = (1,(a=1 && b))
             
let f2 a = a=a
           
let f3 a a = 1
  
let f4 (a,b) = (b,a)
               
let f5 x y a = y a (x a)
  
let f6 (x,y,a) = y(a ,(x a))