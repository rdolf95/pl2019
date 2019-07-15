datatype pattern = Wildcard | Variable of string | UnitP
                  | ConstP of int | TupleP of pattern list
                  | ConstructorP of string * pattern

datatype valu = Const of int | Unit | Tuple of valu list 
               | Constructor of string * valu

fun check_pat(pat) =
    let
        fun check_list p_list =
            case p_list of
                  [] => false
                | p::p_list' => List.exists (fn y=> p = y) p_list' orelse check_list p_list'
    in
        case pat of
              TupleP tuple_pat => (let fun get_elements xs =
                                        case xs of
                                              [] => []
                                            | (TupleP x)::xs' => (get_elements x)@(get_elements xs')
                                            | (ConstructorP (_,y))::ys' =>(get_elements (y::[]))@(get_elements ys')
                                            | (Variable z)::zs' => z::(get_elements zs')
                                            | _ ::as' => get_elements as'
                                            
                                  in
                                    not(check_list(get_elements tuple_pat))
                                  end)
            | ConstructorP (str, patt) => check_pat(patt)
            | _ => true

        end
(*
not (check_list (foldl (fn (x,y) => 
                case x of Variable x => x::y | _ => y) [] tuple_pat))*)

(*tuple 일때 확인해야함*)


fun match (value, pat) = 
    case (value,pat) of
          (_, Wildcard) => SOME []
        | (_, Variable var) => SOME [(var,value)]
        | (Unit, UnitP) => SOME []
        | (Const cons_val, ConstP cons_pat) => SOME []
        | (Tuple tup_val, TupleP tup_pat) => 
            if List.length tup_val = List.length tup_pat
            then
            let
                val lists_pair = ListPair.zip(tup_pat, tup_val)
            in
                if lists_pair = List.filter (fn (x,y) => Option.isSome(match(y,x))) lists_pair
                then 
                    let fun concat_tuples pair_list =
                        case pair_list of
                              [] => []
                            | (x,y)::pairs => Option.valOf(match(y,x)) @ concat_tuples(pairs)
                    in
                        SOME (concat_tuples(lists_pair))
                    end
                else NONE
            end
            else
                NONE
            

        | (Constructor(s1, constr_val), ConstructorP(s2, constr_pat)) => 
            if s1 = s2 then match(constr_val, constr_pat) else NONE
        | _ => NONE


type name = string
datatype RSP =
      ROCK
    | SCISSORS
    | PAPER

datatype 'a strategy = Cons of 'a * (unit -> 'a strategy)

datatype tournament =
      PLAYER of name * (RSP strategy ref)
    | MATCH of tournament * tournament

fun next(strategyRef) =
        let val Cons(rsp, func) = !strategyRef 
        in
            strategyRef := func();
            rsp 
        end

fun whosWinner(t) =
    let
        fun duel(PLAYER (nm1,strategy1), PLAYER(nm2, strategy2)) =
            case (next strategy1, next strategy2) of
                  (ROCK, SCISSORS) => PLAYER (nm1,strategy1)
                | (ROCK, PAPER) => PLAYER (nm2,strategy2)
                | (SCISSORS, ROCK) => PLAYER (nm2,strategy2)
                | (SCISSORS, PAPER) => PLAYER (nm1,strategy1)
                | (PAPER, SCISSORS) => PLAYER (nm2,strategy2)
                | (PAPER, ROCK) => PLAYER (nm1,strategy1)
                | _ => duel (PLAYER (nm1,strategy1), PLAYER(nm2, strategy2))

        (*
            case (!strategy1, !strategy2) of

            
                  (Cons(ROCK, _), Cons(SCISSORS, _)) => Player (nm1,strategy1)
                | (Cons(ROCK, _), Cons(PAPER, _)) => Player (nm2,strategy2)
                | (Cons(SCISSORS, _), Cons(ROCK, _)) => Player (nm2,strategy2)
                | (Cons(SCISSORS, _), Cons(PAPER, _)) => Player (nm1,strategy1)
                | (Cons(PAPER, _), Cons(SCISSORS, _)) => Player (nm2,strategy2)
                | (Cons(PAPER, _), Cons(ROCK, _)) => Player (nm1,strategy1)
                | _ => duel(

                *)
    in
        case t of 
              PLAYER pl => t 
            | MATCH (t1,t2) => duel(whosWinner t1, whosWinner t2)

    end

fun onlyOne(one:RSP) = 
    Cons(one, fn() => onlyOne(one))

fun alterTwo(one:RSP, two:RSP) = 
    Cons(one, fn() => alterTwo(two, one))

fun alterThree(one:RSP, two:RSP, three:RSP) = 
    Cons(one, fn() => alterThree(two, three, one))

(*
val r = onlyOne(ROCK)
val s = onlyOne(SCISSORS)
val p = onlyOne(PAPER)
val rp = alterTwo(ROCK, PAPER)
val sr = alterTwo(SCISSORS, ROCK)
val ps = alterTwo(PAPER, SCISSORS)
val srp = alterThree(SCISSORS, ROCK, PAPER)

val player1 = PLAYER("1", ref r)
val player2 = PLAYER("2", ref s)
val player3 = PLAYER("3", ref p)
val player4 = PLAYER("4", ref rp)
val player5 = PLAYER("5", ref sr)
val player6 = PLAYER("6", ref srp)


val winner1 = whosWinner(MATCH(player4,MATCH(player1, player3)))
val winner2 = whosWinner(MATCH(player1, player4))
val winner3 = whosWinner(MATCH(player4, MATCH(player5, player6)))
*)


val var1 = Variable "qqqqqqqq"
val var2 = Variable "ssssssssss"
val var3 = Variable "sdsdddddddddd"
val tup = TupleP [var1,var2,var3,UnitP, Wildcard]
val tup2 = TupleP [var1,var2,var3,UnitP, Wildcard, var1]

val tup4 = TupleP[var1,var2,UnitP,Wildcard,var3]
val tup5 = TupleP[UnitP,Wildcard,var3]
val tup3 = TupleP [tup4,tup5]
val tupv = Tuple [Unit, Const 10, Const 20, Unit, Unit]
val tupv2 = Tuple [Unit, Const 10, Const 20, Unit, Unit, Const 17]
val tupv3 = Tuple [Unit, Const 10, Const 20, Const 3, Unit, Const 17]
val var4 = ConstructorP ("hello", ConstP 15)
val varv4 = Constructor ("hello", Const 15)
val var5 = ConstructorP ("hellooo", ConstP 15)
val var6 = ConstructorP ("hellooo", Variable "aaaaaaa")
val varv5 = Constructor ("hellooo", Const 15)


