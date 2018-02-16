(* Given a sorted array to implement binary search *)

open Array;

(* A[!lo..!hi-1] is the slice of the array A that needs to be searched 
   !mid is the value returned -- !mid = n if x is not in the array.

   Assume R is a total order on the domain of A and A is sorted according to R
*)
fun binsearch (R, A, x) = 
    let val n = length A;
	  val lo = ref 0 and hi = ref n;
	  val mid = ref ((!lo + !hi) div 2);
    in  (* INV: ordered A /\ (forall i: 0<= i < lo: x <> A[i]) /\
	          (forall j: hi <= j < n: x <> A[j]) /\ 
                0<=!lo <= n /\ 0 <= !hi<=n 
        *)
	  while ((!lo < !hi) andalso (x <> sub (A, !mid))) do
	  (
	    if R(x, sub (A, !mid)) then hi := !mid
	    else (* x < A[!mid] *) lo := !mid + 1;
	    mid := (!lo + !hi) div 2
	   );
        if (!lo > !hi) then n else !mid
    end
