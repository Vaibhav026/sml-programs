fun merge (L1 ,L2 as [],A) =
   	rev( (rev L1) @ A )

   | merge (L1 as [],L2,A) = 
	 rev( (rev L2) @ A )	   	 	
	
   | merge (L1 as h::t,L2 as h1::t1,A) =
     if(h<= h1) then
     merge(t,L2,h::A)
 	 else
 	 merge(L1,t1,(h1)::A) 	 

















