--Creates function "primes_lt x" that returns all Ints y < x s.t. y can expressed as the sum of prime numbers

prime_finder::(Integral a)=>[a]->[a]
prime_finder [] = []
prime_finder (x:xs)
    |x<=2 = 2:prime_finder[x| x<-xs, x>=3]
    |elem True (map (\a->mod x a==0) [2..(x-1)]) = prime_finder xs
    |otherwise = x:(prime_finder xs)

--takes a list a and list of lists b and places elements of a into respective list b elementwise
myzip::[a]->[[a]]->[[a]]
myzip _ [] = []
myzip [] _ = []
myzip (x:xs) (y:ys) = (x:y):(myzip xs ys)

--takes a list and returns list of list, each element in a list
init_lists::[a]->[[a]]
init_lists xs = map (\x->[x]) xs


--returns all possible sequences of n elements in (k,k-1,...,1) groups
--Note: second and third param must be identical
n_choose::(Integral a)=>a->[b]->[b]->[[[b]]]
n_choose _ [] _ = []
n_choose _ _ [] = []
n_choose k (x:xs) ys
  |k<=1 = []
  |null (x:xs) = n_choose (k-1) ys ys
  |k==2 = (myzip (repeat x) (init_lists (x:xs))):(n_choose k xs ys)
  |otherwise = (myzip (repeat x) append_lst):(n_choose k xs ys) where append_lst = foldl (\a b->a++b) [] (n_choose (k-1) (x:xs) ys)

--quick fix to the fact that the same parameter is passed twice in n_choose
n_choose_function::(Integral a)=>a->[b]->[[b]]
n_choose_function k xs = foldl (\a b->a++b) [] (n_choose k xs xs)

--returns unique elements from list
my_unique::(Eq a)=>[a]->[a]
my_unique [] = []
my_unique (x:xs)
  |elem x xs = my_unique xs
  |otherwise = x:my_unique xs

--Main function applies n_choose k through n_choose 1 to the list, then sum's every possible seq value
main_function::(Num b, Integral a)=>a->[b]->[[b]]
main_function 0 _ = []
main_function _ [] = []
main_function iteration_num xs = (map sum (n_choose_function iteration_num xs)):(main_function (iteration_num -1) xs)


--Primes_lt is the end function that returns all ints lt z such that every int is the sum of primes
primes_lt z= [x|x<-(my_unique (main_function_fix (main_function (length (prime_nums)) (prime_nums)))), x<=z] where prime_nums = prime_finder([1..z])

--fixes that main function creates an extra list (why?)
main_function_fix main_rez = foldl (\a b->a++b) [] main_rez

--remove first appearance of element m from list
rm_elem m [] = []
rm_elem m (x:xs)
  |m==x = xs
  |otherwise = x:(rm_elem m xs)

--determines whether two lists are equal, ie same exact elements regardless of order
list_eq [] [] = True
list_eq _ [] = False
list_eq [] _ = False
list_eq (x:xs) ys
  |elem x ys = list_eq xs (rm_elem x ys)
  |otherwise = False

--filters list of lists to only unique lists
func_applier [] = []
func_applier (x:xs)
  |elem True (map (list_eq x) xs) = func_applier xs
  |otherwise = x:(func_applier xs)

--quicksort algorithm
myquicksort::(Ord a)=>[a]->[a]
myquicksort(x:[]) = x:[]
myquicksort [] = []
myquicksort (x:xs)= (myquicksort [n| n<-xs, n<x]) ++ [x] ++ (myquicksort [n| n<-xs, n>=x])


--A far simpler solution

ssp x = length (takeWhile (<x) (scanl1 (+) (prime_finder [1..])))
