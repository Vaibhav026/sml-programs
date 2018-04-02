open String;
open Char;
fun lick (filename:string) =
    let val f = TextIO.openIn filename
        fun loop (accum: string list) =
            case (TextIO.inputLine f) of 
		NONE => accum
              | SOME line => loop (line::accum)
            (* esac *)
        val lines =   rev(loop [])
    in TextIO.closeIn f; lines
    end

fun bite (filename:string) =
    let val f = TextIO.getInstream(TextIO.openIn filename)
    fun loop (accum: string list, f) =
        case TextIO.StreamIO.input f
         of ("", f')    => (TextIO.StreamIO.closeIn f'; accum)
          | (chunk, f') => loop (chunk::accum, f')
            (* esac *)
    in  rev(loop ([], f))
    end


fun count(s,n)=
    if(s=[]) then
    (n,[])    
    else if(ord(hd s) = 35) then
    count(tl s,n+1)
    else
    (n,tl s)    

fun undercount(s,count)=
        if(s = []) then
        count
        else if( ord(hd s) = 95) then
        undercount((tl(s)),count+1)    
        else
        undercount(tl(s),count)   

fun Isline(s,count)=
        if(s = []) then
        false    
        else if(ord (hd s) = 45) then
        (   
            if(count =2) then
            true
            else    
            Isline(tl s,count+1)
        
        )
        else if(ord (hd s) = 32) then
        Isline(tl s,count)
        else
        false         

fun rdlink(s,acc) =
    if(ord (hd s) = 62) then
    (rev acc , tl s)
    else 
    rdlink( tl s, (hd s)::acc )    

fun tonelinktext(s) =
    if(ord (hd s) = 93 ) then    
    []
    else
    hd s :: tonelinktext(tl s)

fun tonelink(s,acc,flag) =
    if( ord (hd s) = 41 ) then
    (tl s , rev acc)    
    else if( ord (hd s) = 40 ) then
    tonelink(tl s,acc,true)    
    else if(flag = true) then
    tonelink(tl s,hd s ::acc,true)
    else
    tonelink(tl s,acc,flag)    

(*Returns the remanant and link *)









        
(*s is a char list for a particular line*)
fun filewrite(s,flag,acc,aster,ucount,tcount)=
    if(s = []) then
    acc    
    else if(flag = true) then
    (   let
        val strng = implode(s)
        in    
        if(isSubstring "http" strng ) then
        (
            let
                val (lnk,left) = rdlink(s,[]) 
            in
                   filewrite(left,false,acc@explode("a href=\" ")@lnk@explode(" \" >")@lnk@explode("</a>"),aster,ucount,tcount) 
            end
            
        )    
        else if(ord(hd s) = 62 ) then
        filewrite(tl s,false,acc @ [hd s],aster,ucount,tcount)    
        else
        filewrite(tl s,true,acc @ [hd s],aster,ucount,tcount)    
        
        end
    )
    else if(ord(hd s) = 60) then
    filewrite(tl s,true,acc @ [hd s],aster,ucount,tcount) 
    else if(ord(hd s) = 35 ) then 
    (   
        let
            val (x,l) = count(s,0)
        in
        
        if( x = 6 ) then
        explode("<h6>") @ l @ explode("</h6>")
        else if(x = 5 ) then
        explode("<h5>") @ l @ explode("</h5>")
        else  if(x = 4) then
        explode("<h4>") @ l @ explode("</h4>")
        else  if(x = 3) then
        explode("<h3>") @ l @ explode("</h3>")
        else  if(x = 2) then
        explode("<h2>") @ l @ explode("</h2>")
        else    
        explode("<h1>") @ l @ explode("</h1>")    
        
        end
            
    )
    else if(ord (hd s) = 42) then
    (
        if(ord (hd (tl s)) = 42 ) then
        (   
            if(aster = false) then    
            filewrite(tl (tl s),false,acc@explode("<strong>"),true,ucount,tcount)
            else
            filewrite(tl (tl s),false,acc@explode("</strong>"),false,ucount,tcount)    
        
        )    
        else
        (
            if(aster = false) then    
            filewrite(tl s,false,acc@explode("<em>"),true,ucount,tcount)
            else
            filewrite( (tl s),false,acc@explode("</em>"),false,ucount,tcount)    
        
        )    
            
    )
    else if(ord (hd s) = 95 ) then 
    (   
        if( ucount = 0 ) then
        filewrite( tl s ,flag, acc@explode("<u>"),aster,ucount+1,tcount)    
        else if( ucount = tcount-1 ) then    
        filewrite( tl s ,flag , acc@explode("</u>"),aster,ucount+1,tcount)
        else
        (
            filewrite(tl s, flag , acc , aster,ucount+1,tcount )
        )        
    )
    else if(ord (hd s) = 91 ) then
    (
        let
            val text = tonelinktext(tl s)
            val (leftover,link) = tonelink(s,[],false)
        in
            filewrite(leftover, flag , acc@explode("<a href=\" ")@link@explode(" \"> ")@text@explode("</a>") , aster,ucount,tcount )
        end
    )    
    else    
    filewrite( tl s,flag,acc @ [hd s],aster,ucount,tcount )    

fun checkstarttable(s)=
    if(s=nil) then
    false    
    else if(ord(hd s) = 32 orelse ord(hd s) = 9 ) then 
    checkstarttable(tl s)    
    else if(ord(hd s) = 60 andalso (tl s) <> [] andalso ord(hd(tl s)) = 60 ) then 
    true
    else 
    false  

fun checkendtable(s)=
    if(s=nil) then
    false
    else if(ord(hd s) = 32 orelse ord(hd s) = 9) then 
    checkendtable(tl s)    
    else if(ord(hd s) = 62 andalso (tl s) <> [] andalso ord(hd(tl s)) = 62 ) then 
    true
    else 
    false

fun ispace(c)=
    if(ord c = 32) then
    true
    else
    false    

fun isspaceline(L)=
    if(L = nil orelse ord (hd L) = 10  ) then
    true
    else if(ispace(hd L) orelse ord(hd L) = 9) then
    isspaceline(tl L) 
    else
    false

        
fun usefilew(str(*,istbl*))=
    let
        (*val isolist =*)      
        val tc = undercount(explode(str),0)
        val linecheck = Isline(explode(str),0)
        val linkcheck1 = isSubstring "http" str
        val space = isspaceline(explode(str))
    in
        if(space) then
        "<p></p>"     
        else if(linecheck = true) then
        "<hr>"
        else    
        implode(filewrite(explode(str),false,[],false,0,tc))
    end


fun append (filename: string, s) = 
    let val f =  TextIO.openAppend filename
    in  (TextIO.output (f, s); TextIO.closeOut f) 
    end



fun appendLine  (filename: string, s) = 
    let val f =  TextIO.openAppend filename
    in  (TextIO.output (f, s^"\n"); TextIO.closeOut f) 
    end


fun a(L:int)=
appendLine("read","Hi")


fun b()=
appendLine("read","Bye")


    
fun extractquote(S)=
    if(S = nil ) then
    []    
    else if((ord (hd S)) = 32 orelse (ord (hd S)) = 9  orelse (ord (hd S)) = 62 ) then
    extractquote(tl S)    
    else
    S
        
fun quo(i) =
    if( i = 0) then
    "\n"
    else
    "</blockquote>" ^ quo(i-1)    


fun isstrtlst(s)=
    if(s = nil) then
    (false,[])    
    else if( isDigit(hd s) ) then
    isstrtlst(tl s)    
    else if(ord(hd s) = 46) then
    (true,tl s)
    else 
    (false,[])    

fun isquote(s) = 
     if(s = nil) then
      false  
     else if(ord(hd s) = 62) then
     true
     else 
     false   


fun drawTable(a,bol,strt) =
    if(strt) then
    appendLine("read.html","<CENTER><TABLE border=\"1\"> ")    
    else if(bol) then
    appendLine("read.html","</TABLE></CENTER>")
    else if(a = nil) then
    appendLine("read.html"," ")
    else if( isDigit(hd a) andalso isspaceline(tl a) ) then
    append("read.html",implode([hd a])^"</TD> </TR>")  
    else
    (
        if( ispace(hd a) andalso isDigit( hd(tl a) ) ) then
        ( let
            val name = append("read.html","<TR><TD>" )
          in
              drawTable(tl a,bol,strt)
          end
        )    
        else if( isDigit (hd a) ) then
        ( let
            val name = append("read.html",implode([hd a]) )
          in
              drawTable(tl a,bol,strt)
          end
        )
        else if(ord(hd a) = 124 ) then
        (
          let
            val name = append("read.html","</TD><TD>" )
          in
              drawTable(tl a,bol,strt)
          end
        )    
        else
        (
            
          let
            val name = append("read.html"," " )
          in
              drawTable(tl a,bol,strt)
          end
        )
    )    

fun fi(L,is,quotcount)=
  
if(L = nil) then
1
else if(is  orelse checkstarttable(explode(hd L)) ) then
(   
    let
        val a=explode(hd L)
        val endtable  = checkendtable(a)
        val starttable = checkstarttable(a)
        val d = drawTable(a,endtable,starttable)
    in
        fi(tl L,not(endtable),quotcount)
    end


)
else if(hd L="start;\n") then
(   
    let
        val m=appendLine("read.html","<ol>")
    in
         fi(tl L,is,quotcount)   
    end
    
)
else if( #1( isstrtlst(explode(hd L)) ) = true ) then
(   
    let
        val (b,rem) = isstrtlst( explode(hd L) )
        val conv = usefilew(implode(rem))
        val m = appendLine("read.html","</li><li>"^conv)
    in
         fi(tl L,is,quotcount)   
    end

)
else if(hd L="end;\n") then
(
    let
        val m=appendLine("read.html","</li></ol>")
    in
         fi(tl L,is,quotcount)   
    end

)
else if(isquote( explode(hd L) ) ) then
(
    let
        val na = appendLine("read.html","<blockquote>")

        val e=  implode( extractquote( explode(hd L) ) )                   (*extracts leftover after this '>' ,then we will append that string again in L so that it is treated normally after that*)
    in
      fi(e::tl L,is,quotcount+1)       
    end


)
else if(hd L="bend;\n") then
(
    let
        val strng = quo(quotcount)
        val  m = appendLine("read.html",strng)
    in
        fi(tl L,is,0)
    end

)    
else
(   
   let  
       val conv = usefilew(hd L)
       val m = appendLine("read.html",conv)
    in
            
        fi(tl L,is,quotcount)
    end 
) 


fun ans()=
let
     val file=lick("mdtab.txt")
in
    fi(file,false,0)
end

    

fun modify(L) =
	if(L = []) then
	[]
	else	
	("<p>" ^ hd L ^ "</p>")::modify(tl L)

(*val a =modify(file);*)

(*val b= isSubstring "abc" "abcd" ;*)

(*fun process(s)=
    if(isSubstring("######",s)) then
    "<h6>" ^ s ^ "</h6>"
    else if(isSubstring("#####",s)) then
    "<h5>" ^ s ^ "</h5>"
    else if(isSubstring("####",s)) then
    "<h4>" ^ s ^ "</h4>"
    else if(isSubstring("###",s)) then
    "<h3>" ^ s ^ "</h3>"
    else if(isSubstring("##",s)) then
    "<h2>" ^ s ^ "</h2>"
    else if(isSubstring("#",s)) then
    "<h1>" ^ s ^ "</h1>"
    else
    s    
*)	
	







