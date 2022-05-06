let min a b = if a < b then a else b;;


let rec gcd a b = if b = 0 then a else gcd b (a mod b);;