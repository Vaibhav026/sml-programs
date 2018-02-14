
fun min([]) = 1000000
	|  min (h::t) =

		let 
		val a=min(t)

	in    if( h < a ) then 
	      h
	      else 		
	      a	
	end
	
fun  del([],A,x) = []
	|del(h::t,A,x)=
		if h=x  then
		(rev A) @ t 
		else
		del(t,h::A,x);	

fun sort([],A)=A
    
    | sort(L,A) =

let 
	val a=min(L);
	val b=del(L,[],a)   

in 
	sort(b,a::A)
end	