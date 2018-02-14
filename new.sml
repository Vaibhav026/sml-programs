fun merge2 (L1,L2 as []) =
	L1
    | merge2(L1 as [],L2)=
    L2
    | merge2(L1 as h1::t1,L2 as h2::t2)=
    if(h1<h2) then
    h1::merge2(t1,L2)
	else
	h2::merge2(L1,t2)	


fun split ([h],A,i,l)=([h],[])	

   | split (L as h::t,A,i,l)=
   		
   		if(i=l) then 
   		(rev A,L)
   		else
   		split(t,h::A,i+1,l)

fun ans(L) =

	let
		val l= (length L) div 2
	 	  
	 in
	 	split(L,[],0,l)
	 end 


   		
		