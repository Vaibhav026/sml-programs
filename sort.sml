(* It is assumed that in all the following algorithms the ordering relation
   R is a binary total ordering. Otherwise there is no guarantee that these 
   programs will work correctly.
*)

(*---------------------------- INSERTION SORT ---------------------- *)
fun insertSort R [] = []
  | insertSort R (h::t) =
        let fun insert R [] x = [x]
              | insert R (h::t) x = if R (x, h) then x::(h::t)
                                  else h::(insert R t x)
          val rest = insertSort R t
      in  insert R rest h
      end;

(* Test
val i = insertSort;
i (op <) [~12, ~24, ~12, 0, 123, 45, 1, 20, 0, ~24];
i (op <=) [~12, ~24, ~12, 0, 123, 45, 1, 20, 0, ~24];
i (op >) [~12, ~24, ~12, 0, 123, 45, 1, 20, 0, ~24];
i (op >=) [~12, ~24, ~12, 0, 123, 45, 1, 20, 0, ~24];
*)

(* --------------------------- SELECTION SORT ------------------------- *)

fun selSort R [] = []
  | selSort R [h] = [h]
  | selSort R (L as h::t) = 
    let 
	  exception emptyList;
	  (* findMin finds the minimum element in the list and removes it *)
	  fun findMin R [] = raise emptyList
	    | findMin R [h] = (h, [])
	    | findMin R (h::t) = 
		let
		    val (m, tt) = findMin R t;

		in  if R(m, h) then (m, h::tt) else (h, t)
		end;
	  val (m, LL) = findMin R L
     in m::(selSort R LL)
    end;

(* Test
val s = selSort;
s (op <) [~12, ~24, ~12, 0, 123, 45, 1, 20, 0, ~24];
s (op <=) [~12, ~24, ~12, 0, 123, 45, 1, 20, 0, ~24];
s (op >) [~12, ~24, ~12, 0, 123, 45, 1, 20, 0, ~24];
s (op >=) [~12, ~24, ~12, 0, 123, 45, 1, 20, 0, ~24];

*)


(* --------------------------- BUBBLE SORT ------------------------- *)

fun bubble R [] = []
  | bubble R [h] = [h]
  | bubble R (f::s::t) = (* can't bubble without at least 2 elements *)
           if R (f, s) then f::(bubble R (s::t))
           else s::(bubble R (f::t));

fun unsorted R [] = false
  | unsorted R [h] = false
  | unsorted R (f::s::t) = 
        if (f=s)  (* if R is an irreflexive relation remove duplicates *)
      then (unsorted R (s::t))
      else if R (f, s) then (unsorted R (s::t)) else true;

(* If R is not a total order then unsorted will yield false in any list 
   containing incomparable elements.
*)

fun bubbleSort R L = if (unsorted R L) then (bubbleSort R (bubble R L)) else L;

(* Test
val b = bubbleSort;
b (op <) [~12, ~24, ~12, 0, 123, 45, 1, 20, 0, ~24];
b (op <=) [~12, ~24, ~12, 0, 123, 45, 1, 20, 0, ~24];
b (op >) [~12, ~24, ~12, 0, 123, 45, 1, 20, 0, ~24];
b (op >=) [~12, ~24, ~12, 0, 123, 45, 1, 20, 0, ~24];

*)

(* Alternatively we can use the generalized "while" defined below *)

fun wh b f x =  if (b x) then (wh b f) (f x) else x;

fun bubbleSortW R = wh (unsorted R) (bubble R);

(* Test
val bw = bubbleSortW;
bw (op <) [~12, ~24, ~12, 0, 123, 45, 1, 20, 0, ~24];
bw (op <=) [~12, ~24, ~12, 0, 123, 45, 1, 20, 0, ~24];
bw (op >) [~12, ~24, ~12, 0, 123, 45, 1, 20, 0, ~24];
bw (op >=) [~12, ~24, ~12, 0, 123, 45, 1, 20, 0, ~24];
*)


(* ------------------------ MERGE SORT ---------------------------------- *)
fun mergeSort R [] = []
  | mergeSort R [h] = [h]
  | mergeSort R L = (* can't split a list unless it has > 1 element *)
       let fun split []  = ([], [])
             | split [h] = ([h], [])
             | split (h1::h2::t) = 
                   let val (left, right) = split t;
                    in (h1::left, h2::right)
                   end;
           val (left, right) = split L;
           fun merge (R, [], []) = []
             | merge (R, [], (L2 as h2::t2)) = L2
             | merge (R, (L1 as h1::t1), []) = L1
             | merge (R, (L1 as h1::t1), (L2 as h2::t2)) =
                      if R(h1, h2) then h1::(merge (R, t1, L2))
                      else h2::(merge(R, L1, t2));
           val sortedLeft = mergeSort R left;
           val sortedRight = mergeSort R right;
       in  merge (R, sortedLeft, sortedRight) 
       end;

(* Test 
val m = mergeSort;
m (op <) [~12, ~24, ~12, 0, 123, 45, 1, 20, 0, ~24];
m (op <=) [~12, ~24, ~12, 0, 123, 45, 1, 20, 0, ~24];
m (op >) [~12, ~24, ~12, 0, 123, 45, 1, 20, 0, ~24];
m (op >=) [~12, ~24, ~12, 0, 123, 45, 1, 20, 0, ~24];
*)

(* ----------------------------- QUICK SORT -------------------------- *)
fun quickSort R [] = []
  | quickSort R (h::t) = 
            let fun part R p [] = ([], [])
                  | part R p (f::r) = 
                       let val (lesser, greater) = part R p r
                        in  if R (f, p) then (f::lesser, greater)
                            else (lesser, f::greater)
                       end
                val (left, right) = part R h t;
                val sortedLeft = quickSort R left;
                val sortedRight = quickSort R right;
            in  sortedLeft @ (h::sortedRight)
            end;

(* Test
val q = quickSort;
q (op <) [~12, ~24, ~12, 0, 123, 45, 1, 20, 0, ~24];
q (op <=) [~12, ~24, ~12, 0, 123, 45, 1, 20, 0, ~24];
q (op >) [~12, ~24, ~12, 0, 123, 45, 1, 20, 0, ~24];
q (op >=) [~12, ~24, ~12, 0, 123, 45, 1, 20, 0, ~24];
*)

(* ==================================================================== *)


(* Some non-integer examples *)
(* Lexicographic ordering on strings *)

fun lexlt (s, t) =
    let val Ls = explode (s);
        val Lt = explode (t);
        fun lstlexlt (_, []) = false
        |   lstlexlt ([], (b:char)::M) = true
        |   lstlexlt (a::L, b::M) = 
                   if (a < b) then true 
                   else if (a = b) then lstlexlt (L, M)
                        else false
        ;
     in lstlexlt (Ls, Lt)
     end
;

fun lexleq (s, t) = (s = t) orelse lexlt (s, t);

fun lexgt (s, t) = lexlt(t, s);

fun lexgeq (s, t) = (s = t) orelse lexgt (s, t);

val stars = ["Kajol", "Madhuri", "Aishwarya", "Amisha", "Urmila", "Juhi", "Raveena", "Sonali"];

(*
i  lexlt stars;
b  lexlt stars;
bw lexlt stars;
m  lexlt stars;
s  lexlt stars;
q  lexlt stars;

i  lexleq stars;
b  lexleq stars;
bw lexleq stars;
m  lexleq stars;
s  lexleq stars;
q  lexleq stars;

i  lexgt stars;
b  lexgt stars;
bw lexgt stars;
m  lexgt stars;
s  lexgt stars;
q  lexgt stars;

i  lexgeq stars;
b  lexgeq stars;
bw lexgeq stars;
m  lexgeq stars;
s  lexgeq stars;
q  lexgeq stars;
*)