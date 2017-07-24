(def fib (n)
  (if (<= n 2) 1
   else (+ (fib (- n 1)) (fib (- n 2)))))

(def factorial (n)
 (if (== n 0) 1
  else (* n (factorial (- n 1)))))

(def head (l)
  (!! l 0))

(def empty (a) (== () a))

(def take (n ls)
  (if (empty ls) ()
   else (if (== n 0) ()
         else (cons (head ls) (take (- n 1) (tail ls))))))

(def drop (n ls)
  (if (empty ls) ()
   else (if (== n 0) ls
         else (drop (- n 1) (tail ls)))))

(def ^ (b e)
  (if (== e 0) 1
   else (* b (^ b (- e 1)))))

(def even (n)
  (== (% n 2) 0))

(def && (a b)
  (if a
    (if b True else False)
   else False))

(def || (a b)
  (if a True
   else (if b True else False)))

(def length (ls)
  (if (empty ls) 0
   else (+ 1 (length (tail ls)))))

(def map (f:f ls)
  (if (empty ls) ()
   else (cons (f (head ls)) (map f (tail ls)))))

(def filter (f:f ls)
  (if (empty ls) ()
   else (if (f (head ls))
          (cons (head ls) (filter f (tail ls)))
         else
          (filter f (tail ls)))))

(def foldl (op:f current ls)
  (if (empty ls) current
   else (foldl op (op current (head ls)) (tail ls))))

(def not (v) (if v False else True))

(def any (f:f ls)
  (if (empty ls) False
   else (if (f (head ls)) True
         else (any f (tail ls)))))

(def all (f:f ls)
  (if (empty ls) True
   else (if (not (f (head ls))) False
         else (all f (tail ls)))))

(def isdivisible (a b) (== 0 (% a b)))

(def divides (a b) (== 0 (% b a)))

(def flip (f:f a b) (f b a))

(def isprime (n)
  (if (== n 2) True
   else (if (== n 1) False
         else (not (any (isdivisible n) (range 2 3 (/ n 2)))))))

(def gcd (a b)
  (if (== b 0) a
   else (if (> b a) (gcd b a)
         else (gcd (- a b) b))))

(def lcm (a b) (/ (* a b) (gcd a b)))
