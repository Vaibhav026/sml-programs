fun checkprime(x,i) =
    if((i*i)>x) then
    0
    else if( (x mod i)=0 ) then
    i
    else
    checkprime(x,i+1)


fun euclid([],L,p) = L

    | euclid(h::t,L,p)=
        
        if( checkprime((p*h)+1,2)=0 ) then
        euclid ( t , L, p*h )
        else
        euclid( t,( (p*h)+1 , checkprime((p*h)+1,2) )::L,p*h )



(*in euclid([2,3,5....,23],[],1)*) 