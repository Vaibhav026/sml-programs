fun isprime(i,[])=true
    | isprime (i,(h::t))=
        if i mod h=0 then false
        else isprime(i,t)

fun genp(L,i,n) =       
    if(L=[]) then
    genp(i::L,(i+1),n)
    else if(i>n) then
    L
    else if(isprime(i,L)) then
    genp(i::L,(i+1),n)
    else
    genp(L,(i+1),n);