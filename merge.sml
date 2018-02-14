fun merge (L1 ,L2 as [],A) =
   	rev( (rev L1) @ A )

   | merge (L1 as [],L2,A) = 
	 rev( (rev L2) @ A )	   	 	
	
   | merge (L1,L2,A) =
     if(hd L1 <= hd L2) then
     merge(tl L1,L2,(hd L1)::A)
 	 else
 	 merge(L1,tl L2,(hd L2)::A) 	 

















