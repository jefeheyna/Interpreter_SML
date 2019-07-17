fun interpreter(input,output)=
	let

		val ins  = TextIO.openIn "input1.txt"
        val outs = TextIO.openOut "output1.txt"

        datatype Val = 
        	INT of string*string 
        	| STR of string*string
        	| REAL of real
        	| NAME of string*string | BOOL of string*string | ERROR of string*string | NOTHING

        datatype command = Push|Pop|Add|Sub|Mul|Div|Rem|Neg|Swap|Quit|Cat|Or|And|Not|Equal|LessThan|Bind

        fun getfirst(x,y)=x
        fun getlast(x,y)=y

        fun isInts (y,x) = 
        	let
        		val first = Int.fromString(getfirst(y))
        		val second = Int.fromString(getfirst(x))
        	in
        		if first = NONE orelse second = NONE then false else true
        	end
        
        fun isInt(y)=
        	let
        		val first = Int.fromString(getfirst(y))
        	in
        		if first = NONE then false else true
        	end

        fun isBools (y,x) = 
            let
                val first = if getfirst(y)=":true:" orelse getfirst(y)=":false:" then true else false
                val second = if getfirst(x)=":true:" orelse getfirst(x)=":false:" then true else false
            in
                if first andalso second then true else false
            end
        
        fun isBool (y) = 
            let
                val first = if getfirst(y)=":true:" orelse getfirst(y)=":false:" then true else false
            in
                if first then true else false
            end

        fun add(first,second)=
        	let
        		val SOME y = Int.fromString(getfirst(first))
				val SOME x= Int.fromString(getfirst(second))
				val result = x+y
				val result_isneg = 0-result 
        	in
        		if result<0 then ("-"^Int.toString(result_isneg),"INT") else (Int.toString(result),"INT")
        	end	

        fun sub(first,second)=
        	let
        		val SOME y = Int.fromString(getfirst(first))
				val SOME x= Int.fromString(getfirst(second))
				val result = x-y
				val result_isneg = 0-result
				
        	in
        		if result<0 then ("-"^Int.toString(result_isneg),"INT") else (Int.toString(result),"INT")
        	end	
	
        fun mul(first,second)=
        	let
        		val SOME y = Int.fromString(getfirst(first))
				val SOME x= Int.fromString(getfirst(second))
        		val result = x*y
				val result_isneg = 0-result
			 
        	in
        		if result<0 then ("-"^Int.toString(result_isneg),"INT") else (Int.toString(result),"INT")
        	end	

        fun divide(first,second)=
        	let
        		val SOME y = Int.fromString(getfirst(first))
				val SOME x= Int.fromString(getfirst(second))
				val result = x div y
				val result_isneg = 0-result
        	in
        		if result<0 then ("-"^Int.toString(result_isneg),"INT") else (Int.toString(result),"INT")
        	end	

        fun rem(first,second)=
        	let
        		val SOME y = Int.fromString(getfirst(first))
				val SOME x= Int.fromString(getfirst(second))
				val result =  x mod y
				val result_isneg = 0-result
        	in
        		if result<0 then ("-"^Int.toString(result_isneg),"INT") else (Int.toString(result),"INT")
        	end	
        fun neg(first)=
        	let
        		val SOME y = Int.fromString(getfirst(first))
        		val y_alreadyneg = 0-y
                val blah = "-"^Int.toString(y_alreadyneg)
                val blah2 = Int.toString(y)
        	in
        		if y=0 then (Int.toString(0),"INT")
        		else if y<0 then (blah,"INT") else (blah2,"INT")
        	end	

        fun iszero y = Int.fromString(getfirst(y))= SOME 0

        fun checknums mystring=
        	let
        		val x = Int.fromString(getfirst(mystring))
        	in
        		case x of
        			SOME(i) => if i<0 then ("-"^Int.toString (0-i),"INT") else (Int.toString i,"INT")
        			| NONE=> mystring
        	end

        fun andf(first,second)=
            if getfirst(first)=":true:" andalso getfirst(second)=":true:" then (":true:","BOOL") else (":false:","BOOL")

        fun notf(first)=
            if getfirst(first)=":true:" then (":false:","BOOL") else (":true:","BOOL")


        fun orf(first,second)=
            if getfirst(first)=":true:" orelse getfirst(second)=":true:" then (":true:","BOOL") else (":false:","BOOL")

        fun equal(first,second)=
            if getfirst(first)=getfirst(second) then (":true:","BOOL") else (":false:","BOOL")

        fun lessThan(first,second)=
            let
                val SOME y = Int.fromString(getfirst(first))
                val SOME x = Int.fromString(getfirst(second))
            in
                if y > x then (":true:","BOOL") else (":false:","BOOL")      
            end
        
        fun legalbind(y,x)=
            let
                val x_isname = if getlast(x)="UNIT" orelse getlast(x)="STR" orelse getlast(x)="ERROR" orelse getlast(x)="BOOL" then false else true
                val y_value = if getlast(y)="ERROR" orelse getlast(y)="" then false else true
            in
                if x_isname andalso y_value then true else false
            end

        fun bind(y:string*string,x:string*string)= (first(x),second(y))
            
         fun legalcat(y,x)=
            let
                val x_isSTR = getlast(x)="STR"
                val y_isSTR = getlast(x)="STR"
            in
                if x_isSTR andalso y_isSTR then true else false
            end      

        fun interpret(Push,stack,INT(item))= checknums(item)::stack
        | interpret(Push,stack,STR(item))= item::stack
        | interpret(Push,stack,NAME(item))= item::stack
        | interpret(Push,stack,BOOL(item))= item::stack
        | interpret(Push,stack,ERROR(item))= (":error:","ERROR")::stack
        | interpret(Push,stack,REAL(item))= (":error:","ERROR")::stack
        | interpret(Pop,x::xs,z)= xs
        | interpret(Add,y::x::xs,z)= if isInts(y,x) then add(y,x)::xs else (":error:","ERROR")::y::x::xs
        | interpret(Sub,y::x::xs,z)= if isInts(y,x) then sub(y,x)::xs else (":error:","ERROR")::y::x::xs
        | interpret(Mul,y::x::xs,z)= if isInts(y,x) then mul(y,x)::xs else (":error:","ERROR")::y::x::xs
        | interpret(Div,y::x::xs,z)= if isInts(y,x) andalso (not)(iszero y) then divide(y,x)::xs else (":error:","ERROR")::y::x::xs
        | interpret(Rem,y::x::xs,z)= if isInts(y,x) andalso (not)(iszero y) then rem(y,x)::xs else (":error:","ERROR")::y::x::xs
        | interpret(Neg,y::xs,z)= if isInt(y) then neg(y)::xs else (":error:","ERROR")::y::xs
        | interpret(Swap,y::x::xs,z) = x::y::xs
        | interpret(Cat,y::x::xs,z) = if legalcat(y,x) then (first(x)^first(y),"STR")::xs else (":error:","ERROR")::y::x::xs
        | interpret(Not,y::xs,z)= if isBool y then notf(y)::xs else (":error:","ERROR")::y::xs
        | interpret(And,y::x::xs,z)= if isBools(y,x) then andf(y,x)::xs else (":error:","ERROR")::y::x::xs
        | interpret(Equal,y::x::xs,z)= if isInts(y,x) then equal(y,x)::xs else (":error:","ERROR")::y::x::xs
        | interpret(LessThan,y::x::xs,z)=if isInts(y,x) then lessThan(y,x)::xs else (":error:","ERROR")::y::x::xs    
        | interpret(Or,y::x::xs,z)=if isBools(y,x) then orf(y,x)::xs else (":error:","ERROR")::y::x::xs
        | interpret(Bind,y::x::xs,z)=if legalbind(y,x) then (":unit:","UNIT")::bind(y,x)::xs else (":error:","ERROR")::y::x::xs
        | interpret(Quit,stack,z) = stack
        | interpret(_,x,z)= (":error:","ERROR")::x
       
        fun read line=
        	let
        		val thecommand = if String.isPrefix "push" line then Push 
        		else if String.isPrefix "pop" line then Pop
        		else if String.isPrefix "add" line then Add
        		else if String.isPrefix "sub" line then Sub
        		else if String.isPrefix "mul" line then Mul
        		else if String.isPrefix "div" line then Div
        		else if String.isPrefix "rem" line then Rem
        		else if String.isPrefix "neg" line then Neg
        		else if String.isPrefix "swap" line then Swap
                else if String.isPrefix "cat" line then Cat
                else if String.isPrefix "not" line then Not
                else if String.isPrefix "equal" line then Equal
                else if String.isPrefix "and" line then And
                else if String.isPrefix "or" line then Or
                else if String.isPrefix "lessThan" line then LessThan 
                else if String.isPrefix "bind" line then Bind
        		else if String.isPrefix "quit" line then Quit
        		else Quit
        		
        		
        		fun is_Int item =
					case Int.fromString item of
						SOME i => true
						| NONE => false


				fun is_Real item=
					let
						val isNum = is_Int item
					in
						if isNum andalso Char.contains item #"." then true else false
					end


				fun is_String item =
					if Char.contains item #"\"" then true else false


				fun is_Bool item =
					if item = ":true:" orelse item = ":false:" then true else false

				fun is_Error item =
					let
						val myerror = String.compare (":error:",item)= EQUAL
					in
						if myerror then true else false
					end

        		fun getType (Push,item) =
					let
						val subx = Substring.full item
						val subxx = Substring.trimr 1 subx
						val no_nl = Substring.string subxx
						val isInt = is_Int no_nl
						val SOME theInt = if isInt then Int.fromString no_nl else SOME 0
						val isReal = is_Real no_nl
						val SOME theReal = if isReal then Real.fromString no_nl else SOME 0.0
						val isBool = is_Bool no_nl
						val isError = is_Error no_nl
						val isString = is_String no_nl
						val string_noquotes1 = if isString then Substring.full no_nl else subx
						val string_noquotes2 = if isString then Substring.trimr 1 string_noquotes1 else subxx
						val string_noquotes3 = if isString then Substring.triml 1 string_noquotes2 else subxx
						val final_noquotes = if isString then Substring.string string_noquotes3 else no_nl
						val isName = not (is_String no_nl)

					in
						if isInt then INT(no_nl,"INT")
						else if isReal then REAL(theReal)
						else if isBool then BOOL(no_nl,"BOOL")
					 	else if isError then ERROR(no_nl,"ERROR")
						else if isString then STR(final_noquotes,"STR")
						else if isName then NAME(no_nl,"")
						else NOTHING
					end
				
				
				val theType = if thecommand=Push 
				then getType(thecommand,String.extract(line,5,NONE))
				else NOTHING

				
        	in
        		(thecommand,theType)
        end
    
        
        fun readfile i =
            case TextIO.inputLine i of
              SOME s => (s::readfile i)
            | NONE   => []

        fun solve filelist=
        	case filelist of
        		x::xs => (read x)::solve xs
        		| [] => []

        fun first (x,y) = x
		fun second (x,y) = y

        fun solve2 (commandlist,stack)=
        	case commandlist of
        		x::xs => solve2(xs,interpret(first x, stack, second x))
        		| [] => stack

        fun printstack stack=
        	case stack of
        			x::xs => (TextIO.output(outs, first(x) ^ "\n");printstack xs)
        			| [] => (TextIO.closeIn ins; TextIO.closeOut outs)		
    in
        printstack (solve2(solve (readfile ins),[]))
    end