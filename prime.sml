
fun checkprime(x,i) =
    if((i*i)>x) then
    0
    else if( (x mod i)=0 ) then
    i
    else
    checkprime(x,i+1)

fun isprime(i,[])=true
    | isprime (i,(h::t))=
        if i mod h=0 then false
        else isprime(i,t)

fun genp(L,i,product) = 
    
    if(product>100000000) then 
    L
    else if(isprime(i,L)) then
    genp(i::L,(i+2),product*i)
    else
    genp(L,(i+2),product);

fun euclid([],L,p) = L

    | euclid(h::t,L,p)=
        
        if( checkprime((p*h)+1,2)=0 ) then
        euclid ( t , L, p*h )
        else
        euclid( t,( (p*h)+1 , checkprime((p*h)+1,2) )::L,p*h )


fun ans() =

let 
    val B =genp([3,2],5,6)
    val C=rev B

in 
    euclid(C,[],1)
end ;   
                                                                                                         



                   