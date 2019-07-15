fun merge(l1: int list, l2: int list)=
    if null l1 andalso null l2
    then []
    else if null l1
    then hd l2 :: merge(l1,tl l2)
    else if null l2
    then hd l1 :: merge(tl l1, l2)
    else
        if hd l1 > hd l2
        then hd l2 :: merge(l1, tl l2)
        else hd l1 :: merge(tl l1, l2)

fun reverse(x: int list)=
    let 
        fun ret_reverse(reversed:int list, original:int list)=
            if null original
            then reversed
            else ret_reverse(hd original :: reversed, tl original)
    in
        ret_reverse([] , x)
    end

fun sigma(a:int, b:int, f:(int->int))=
    if a=b
    then f(b)
    else f(a)+sigma(a+1,b,f)



fun digits(num:int)=
    let
        fun ret_digits(listed:int list, original:int)=
            if original div 10=0
            then original :: listed
            else ret_digits(original mod 10 ::listed, original div 10)
    in
        ret_digits([],num)
    end
        


fun additivePersistence(num:int)=
    if num div 10=0
    then 0
    else
        let
            fun addDigits(numDigits: int list)=
                if null numDigits
                then 0
                else hd numDigits + addDigits(tl numDigits)

        in
            additivePersistence(addDigits(digits(num)))+1

        end

fun digitalRoot(num:int)=
    if num div 10 = 0
    then num
    else 
        let
            fun addDigits(numDigits: int list)=
            if null numDigits
            then 0
            else hd numDigits + addDigits(tl numDigits)

        in
             digitalRoot(addDigits(digits(num)))
        end
       
