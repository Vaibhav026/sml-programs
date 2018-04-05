fun present((x,y),L) =
	if(L=[]) then
	false
	else if(hd L=(x,y)) then
	true
	else
	present((x,y),tl L)	






fun tour(i,j,L,x,n,con) =

	if(n=64) then 
	L
	else if(i-2>=1 and j-1>=1 and !present((i-2,j-1),L) and ( (x<1 and con)  !con ) ) then
	tour(i-2,j-1,(i,j)::L,1,n+1,false)	
	else if(i-2>=1 and j+1<=8 and !present((i-2,j+1),L) and ( (x<2 and con) or !con ) ) then
	tour(i-2,j+1,(i,j)::L,2,n+1,false)
	else if(i+2<=8 and j+1<=8 and !present((i+2,j+1),L) and ((x<3 and con)  or !con ) ) then
	tour(i+2,j+1,(i,j)::L,3,n+1,false)
	else if(i+2<=8 and j-1>=1 and !present((i+2,j-1),L) and ((x<4 and  con) or !con ) ) then
	tour(i+2,j-1,(i,j)::L,4,n+1,false)
	else
	raise Conflict


	handle Conflict =>
	if(i=1 and j=1) then
	[]	
	else if(x=1)
	tour(i+2,j+1,tl L,x,n-1,true)
	else if(x=2)		
	tour(i+2,j-1,tl L,x,n-1,true)
	else if(x=3)
	tour(i-2,j-1,tl L,x,n-1,true)
	else
	tour(i-2,j+1,tl L,x,n-1,true)	