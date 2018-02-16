fun notpresent((x,y),L) =
	if(L=[]) then
	true
	else if(hd L=(x,y)) then
	false
	else
 	present((x,y),tl L)	 





fun tour(i,j,L,x,n,con) =

	let 
		val a=1

	in
	
	if(n=64) then 
	L
	else if(i-2>=1 andalso j-1>=1 andalso (notpresent((i-2,j-1),L) ) andalso ( (x<1 andalso con) orelse (not con) ) ) then
	tour(i-2,j-1,(i,j)::L,1,n+1,false)	
	else if(i-2>=1 andalso j+1<=8 andalso (notpresent((i-2,j+1),L) ) andalso ( (x<2 andalso con) orelse (not con) ) ) then
	tour(i-2,j+1,(i,j)::L,2,n+1,false)
    else if(i-1>=1 andalso j+2<=8 andalso (notpresent((i-1,j+2),L) ) andalso ( (x<3 andalso con) orelse (not con) ) ) then
	tour(i-1,j+2,(i,j)::L,3,n+1,false)
	else if(i+1<=8 andalso j+2<=8 andalso (notpresent((i+1,j+2),L) ) andalso ( (x<4 andalso con) orelse (not con) ) ) then
	tour(i+1,j+2,(i,j)::L,4,n+1,false)
	else if(i+2<=8 andalso j+1<=8 andalso (notpresent((i+2,j+1),L) ) andalso ((x<5 andalso con)  orelse (not con) ) ) then
	tour(i+2,j+1,(i,j)::L,5,n+1,false)
	else if(i+2<=8 andalso j-1>=1 andalso (notpresent((i+2,j-1),L) ) andalso ((x<6 andalso  con) orelse (not con) ) ) then
	tour(i+2,j-1,(i,j)::L,6,n+1,false) 
	else if(i+1<=8 andalso j-2>=1 andalso (notpresent((i+1,j-2),L) ) andalso  ((x<7 andalso  con) orelse (not con) ) ) then
	tour(i+1,j-2,(i,j)::L,7,n+1,false)
	else if(i-1>=1 andalso j-2>=1 andalso (notpresent((i-1,j-2),L) ) andalso  ( (x<8 andalso  con) orelse (not con) ) ) then
	tour(i-1,j-2,(i,j)::L,8,n+1,false)
	else
	if(i=1 andalso j=1) then
	[]	
	else if(x=1) then
	tour(i+2,j+1,tl L,x,n-1,true)
	else if(x=2) then 		
	tour(i+2,j-1,tl L,x,n-1,true)
	else if(x=3) then
	tour(i-2,j-1,tl L,x,n-1,true)
	else
	tour(i-2,j+1,tl L,x,n-1,true)	
end

	fun ans()=
	tour(1,1,[],0,0,false)
