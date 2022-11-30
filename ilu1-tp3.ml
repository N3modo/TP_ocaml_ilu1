let premierCh n = 
  let rec p1 k = 
    if k < 10 then k 
    else p1 (toutSaufDer k)
  in 
  if n < 0 then failwith "nombre inf 0" 
  else p1 n
  
let toutSaufPrem n =
  let rec san1 k =
    if k < 10 then 0
    else dernierCh(k) + san1(toutSaufDer k) * 10
  in
  if n < 0 then failwith "nombre inf 0"
  else san1 n
      
let rec estPalindrome n =
  (n < 10 && n >= 0) || not(premierCh(abs n) <> dernierCh(abs n)) && estPalindrome(toutSaufPrem(toutSaufDer(abs n)))
  
let rec nbOccs a n =
  if (n < 10 && n >= 0) then 
    if n = a then 1 else 0 
  else nbOccs a (toutSaufDer n) + (if a = (dernierCh n) then 1 else 0)
                                  
let rec iterer n f x =
  if n=0 then x
  else iterer (n-1) f (f x)
      
let id x = x
  
let compose f1 f2 c = 
  (f1 (f2 c))
  
let rec iterer2 n f = 
  if n=0 then id
  else compose f (iterer2 (n-1) f)
    
let rec itererBis f p x =
  if p x then x
  else itererBis f p (f x)
    
let rec qqsoit n p =
  (n<1) || ((p n) && qqsoit (n-1) p)


let div2 x =
  x asr 1
  
let mod2 x =
  x land 1

let rec fastpow n e =
  if e < 0 then failwith "nombre inf 0" 
  else if e = 0 then 1
  else if e = 1 then n
  else if (mod2 e) = 1 then n * fastpow (n * n) (div2 (e-1)) 
  else fastpow (n * n) (div2 e)
          
let rec ack (m, n) =
  if m < 0 || n < 0 then failwith "nombre inf 0" 
  else if m = 0 then (n+1)
  else if n = 0 then ack(m-1, 1)
  else ack(m-1, ack(m, n-1))
















