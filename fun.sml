fun split (L as h::t,i,A,l) =
	if i=(l div 2) then 
   (A,L)	
   else
   split(t,i+1,h::A,l)
  


fun ans(L)=

	let 
		val l=length L;
		val (x,y) = split(L,0,[],l);

	in
	
	((rev x),y)
    
    end	;

fun merge (L1 ,L2 as [],A) =
   	rev( (rev L1) @ A )

   | merge (L1 as [],L2,A) = 
	 rev( (rev L2) @ A )	   	 	
	
   | merge (L1,L2,A) =
     if(hd L1 <= hd L2) then
     merge(tl L1,L2,(hd L1)::A)
 	 else
 	 merge(L1,tl L2,(hd L2)::A) 	


fun mergesort(L as [h])=
	L 
  |	mergesort(L as h::t) =

	 let 
	 	 
		val (L1,L2) = ans(L);


		val Ls1 = mergesort(L1)
		val Ls2 = mergesort(L2)

	 in
	 	
		merge (Ls1,Ls2,[])	

     end; 		
	