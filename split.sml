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