datatype expr = NUM of int
              | PLUS of expr * expr
              | MINUS of expr *expr

datatype formula = TRUE
                 | FALSE
                 | NOT of formula
                 | ANDALSO of formula * formula
                 | ORELSE of formula * formula
                 | IMPLY of formula * formula
                 | LESS of expr * expr

fun eval (form:formula) = 
    let
        fun cal_expr(exp: expr) =
            case exp of
                  NUM(exp1) => exp1
                | PLUS(exp1, exp2) => cal_expr(exp1) + cal_expr(exp2)
                | MINUS(exp1, exp2) => cal_expr(exp1) - cal_expr(exp2)
    in
        case form of
              TRUE => true
            | FALSE => false
            | NOT(form1) => not(eval(form1))
            | ANDALSO(form1, form2) => eval(form1) andalso eval(form2)
            | ORELSE(form1, form2) => eval(form1) orelse eval(form2)
            | IMPLY(form1, form2) => (eval(form1) andalso eval(form2)) orelse (not(eval(form1)) andalso not(eval(form2)))
            | LESS(expr1, expr2) => cal_expr(expr1) < cal_expr(expr2)
    end

type name = string

datatype metro = STATION of name
               | AREA of name * metro
               | CONNECT of metro * metro

fun checkMetro (met: metro) =
    let 
        fun checkMetroA(m, areaList) =
            case m of
                  STATION(sta_name) =>
                    let
                        fun checkName(s, al)=
                            case al of
                                  [] => false
                                | a::al' => s = a orelse checkName(s, al')
                    in
                        checkName(sta_name, areaList)
                    end
                | AREA(area_name, m') => checkMetroA(m', area_name::areaList)
                | CONNECT (m1, m2) => checkMetroA(m1, areaList) andalso checkMetroA(m2, areaList)
    in
        checkMetroA(met, [])
    end



datatype 'a lazyList = nullList
                     | cons of 'a * (unit -> 'a lazyList)


fun seq(first, last) =
    if first < last
    then cons (first, fn () => seq(first+1, last))
    else  (* first == last*)
        cons (last, fn () => nullList)

fun infSeq(first) = 
    cons(first, fn () => infSeq(first+1))

fun firstN(lazyListVal, n) = 
        case lazyListVal of
              nullList => []
            | cons(x, lazyList') => if n>0
                                    then x::firstN(lazyList'(), n-1)
                                    else [] (*n==0*)


fun Nth (lazyListVal, n) =
	case lazyListVal of
		  nullList => NONE
		| cons (x, xs') => if n = 1
						   then SOME(x)
					       else Nth(xs'(), n-1)
												
fun filterMultiples(lazyListVal, n)=
	case lazyListVal of
		  nullList => lazyListVal
		| cons(x, xs) =>
            case xs() of
              nullList =>  if x mod n =0
						   then nullList
					 	   else cons(x,xs)

            | cons(y,ys') =>  if x mod n = 0
							  then filterMultiples(cons(y, ys'), n)
							  else cons(x, fn () => filterMultiples(cons(y, ys'), n))

fun primes () =
	let fun sieve(lazyListVal) =
			case lazyListVal of
				   nullList => lazyListVal  (*i think it isn't neccessary*)
				 | cons(x, xs') => cons(x, fn () => sieve(filterMultiples(xs'(),x)))
	in
		sieve(infSeq(2))
	end


