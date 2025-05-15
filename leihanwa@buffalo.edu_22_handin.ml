type stackValue = BOOL of bool|INT of int| ERROR|STRING of string|NAME of string|UNIT |CLOSURE of (bool*stackValue*(command list)*(stackValue*stackValue)list list)
and command = ADD | SUB | MUL | DIV | PUSH of stackValue|REM|NEG|POP|SWAP|TO_STRING|PRINTLN|QUIT|CAT|AND|OR|NOT|EQUAL|LESS_THAN|BIND|IF|LET|END
              |FUN of (stackValue*stackValue)|IN_OUT_FUN of (stackValue*stackValue)|RETURN|FUN_END|CALL
type basic=(stackValue*stackValue)
type part2=basic list(*come true let..end and bind*)
type combo=part2 list

let interpreter ((input : string), (output : string )) : unit =
let ic =open_in input
in
let oc =open_out output
in
let rec loop_read acc=
  try
    let l= String.trim(input_line ic) in loop_read (l::acc)
  with
    |End_of_file->List.rev acc
in

let strList=loop_read[]
in
let is_Integer s=
try let _ = int_of_string s in 
true
with
|Failure _ ->false(*from offical doc*)
in
(*strList equals inputs, and string*)
let str2sv s=(*str2sv->change strlist conclusion to order*)
match s with
|":true:"-> BOOL(true)
|":false:"->BOOL(false)
|":error:"->ERROR
|":unit:"->UNIT
|s->
  match s.[0]='"' && s.[String.length s-1]='"'with
  |true->STRING(String.sub s 1 (String.length s-2))
  |_->
  match s.[0]='_' || (s.[0]>='a'&&s.[0]<='z')||(s.[0]>='A'&&s.[0]<='Z')with
  |true->NAME(s)
  |_->match (s.[0]>='0'&& s.[0]<='9')||(s.[0]='-'&&s.[1]>='0'&&s.[1]<='9') with
    |true->
      (match s with
      |"-0"->INT(0)
      | s when is_Integer s -> INT(int_of_string s )
      |_->ERROR)
    |_->ERROR
    
in
let str2com s =(*str2com->command*)
  let sr=String.trim s in 
  if String.length sr>=5 && String.sub sr 0 5 ="push "
    then 
      let st = String.sub sr 5 (String.length s-5) in 
  PUSH (str2sv st)
else 
  let st=String.split_on_char ' ' s in
  match st with
  |"fun"::name::arguement::[]->FUN(NAME(name),NAME(arguement))
  |"inOutFun"::name::arguement::[]->IN_OUT_FUN(NAME(name),NAME(arguement))
  |_->
  match s with
  | "add" -> ADD
  | "sub" -> SUB
  | "mul" -> MUL
  | "div" -> DIV
  | "rem"-> REM
  | "neg"->NEG
  |"pop"->POP
  |"swap"->SWAP
  |"toString"->TO_STRING
  |"println"->PRINTLN
  |"quit"->QUIT
  (*part2*)
  |"cat"->CAT
  |"and"->AND
  |"or"->OR
  |"not"->NOT
  |"equal"->EQUAL
  |"lessThan"->LESS_THAN
  |"bind"->BIND
  |"if"->IF
  |"let"->LET
  |"end"->END
  |"return"->RETURN
  |"funEnd"->FUN_END
  |"call"->CALL
  |_->PUSH(ERROR)
in
let sv2str sv=
match sv with
| BOOL(a) -> 
  if a then ":true:"
  else ":false:"
|INT b->string_of_int b
|ERROR-> ":error:"
|STRING c-> c
|NAME d->d
|UNIT->":unit:"
|CLOSURE e->":error:"
in
(*let com2str com =
  match com with
  | ADD -> "add"
  |SUB->"sub"
  |MUL->"mul"
  |DIV->"div"
  |REM->"rem"
  |NEG->"neg"
  |POP->"pop"
  |SWAP->"swap"
  |TO_STRING->"toString"
  |PRINTLN->"println"
  |QUIT->"quit"
  |PUSH co->"push "^ (sv2str co)
in *)
let commands=List.map str2com strList in 
(*part2*)

let rec check (env:part2) (n:stackValue)  =
  match env,n with 
  |[],n->None
  |(NAME(name1),sv)::tail,NAME(name2) when name1=name2 ->Some(sv)
  |_::tail,n->check tail n
  in (*check every single part2*)

let rec check_name (envir:combo) (name:stackValue)=
  match envir with
  | (e1::e2)->(
    match check e1 name with
    | Some value->Some value
    |None->check_name e2 name
  )
  |[]->None(*check all combo get key return value*)
  in

let rec checkFun cmds stack depth=
  match cmds with
  |FUN(x)::tail->checkFun tail (FUN(x)::stack)(depth+1)
  |IN_OUT_FUN(x)::tail->checkFun tail (IN_OUT_FUN(x)::stack) (depth+1)
  |FUN_END::tail when depth=0 ->(List.rev stack ,tail)
  |FUN_END::tail->checkFun tail (FUN_END::stack)(depth-1)
  |cmd::tail->checkFun tail (cmd::stack) depth
  |[]->([],[])(*cse116*)
in

let rec processor (stack:stackValue list list)(cmd:command list list)(env:combo)=
match (stack,cmd,env) with(*idea changed from starflower when I was stuggling in the let...end, I cannot create a new stack, and I watch some posts showing dont overthink,its a good warning for me, follow my first idea*)
  |((s1::stl),((PUSH(STRING(a))::ctl)::cmdtl),env)->processor ((STRING(a)::s1)::stl) (ctl::cmdtl) (env)
  |((s1::stl),((PUSH(INT(a))::ctl)::cmdtl),env)->processor ((INT(a)::s1)::stl) (ctl::cmdtl) (env)
  |((s1::stl),((PUSH(NAME(a))::ctl)::cmdtl),env)->processor ((NAME(a)::s1)::stl) (ctl::cmdtl) (env)
  |((s1::stl),((PUSH(BOOL(a))::ctl)::cmdtl),env)->processor ((BOOL(a)::s1)::stl) (ctl::cmdtl) (env)
  |((s1::stl),((PUSH(UNIT)::ctl)::cmdtl),env)->processor ((UNIT::s1)::stl) (ctl::cmdtl) (env)
  |((s1::stl),((PUSH(ERROR)::ctl)::cmdtl),env)->processor ((ERROR::s1)::stl) (ctl::cmdtl) (env)

  |(((sv1::sv)::stl),((POP::ctl)::cmdtl),env)->processor(sv::stl) (ctl::cmdtl)(env)
  |(([]::stl),((POP::ctl)::cmdtl),env)->processor([ERROR]::stl) (ctl::cmdtl)(env)(*match a with....*)

  |(((INT x1::INT x2::s1)::stl),((ADD::ctl)::cmdtl),env)->processor((INT(x1+x2)::s1)::stl) (ctl::cmdtl) env
  |((INT x1::NAME x2::s1)::stl),((ADD::ctl)::cmdtl),(env)-> 
    (let ny=check_name env (NAME(x2)) in 
    match x1,ny with
      |x1,Some(INT(y1))->processor((INT(x1+y1)::s1)::stl) (ctl::cmdtl) env
      |_->processor((ERROR::INT(x1)::NAME(x2)::s1)::stl)(ctl::cmdtl) env
    )
  |((NAME x1::INT x2::s1)::stl),((ADD::cmdtl)::cmdtltl),(env)-> 
      (let ny=check_name env (NAME(x1)) in 
      match x2,ny with
        |x2,Some(INT(y1))->processor((INT(x2+y1)::s1)::stl) (cmdtl::cmdtltl) env
        |_->processor((ERROR::NAME(x1)::INT(x2)::s1)::stl) (cmdtl::cmdtltl) env
      )
  |((NAME x1::NAME x2::s1)::stl),((ADD::cmdtl)::cmdtltl),(env)->
    (let nx=check_name env (NAME(x1)) in 
    let ny=check_name env (NAME(x2)) in 
    match nx,ny with
      |Some(INT(x1)),Some(INT(y1))->processor((INT(x1+y1)::s1)::stl) (cmdtl::cmdtltl) env
      |_->processor((ERROR::NAME x1::NAME x2::s1)::stl) (cmdtl::cmdtltl) env
    )
  |((s1)::stl),((ADD::cmdtl)::cmdtltl),env->processor((ERROR::s1)::stl) (cmdtl::cmdtltl) env

  |((INT x1::INT x2::s1)::stl),((SUB::cmdtl)::cmdtltl),env->processor((INT(x2-x1)::s1)::stl) (cmdtl::cmdtltl) env
  |((NAME x1::NAME x2::s1)::stl),((SUB::cmdtl)::cmdtltl),(env)->
    (let nx=check_name env (NAME(x1)) in 
    let ny=check_name env (NAME(x2)) in 
    match nx,ny with
      |Some(INT(x1)),Some(INT(y1))->processor((INT(y1-x1)::s1)::stl) (cmdtl::cmdtltl) env
      |_->processor((ERROR::NAME x1::NAME x2::s1)::stl) (cmdtl::cmdtltl) env
    )
  |((INT x1::NAME x2::s1)::stl),((SUB::cmdtl)::cmdtltl),(env)-> 
      (let ny=check_name env (NAME(x2)) in 
      match x1,ny with
        |x1,Some(INT(y1))->processor((INT(y1-x1)::s1)::stl) (cmdtl::cmdtltl) env
        |_->processor((ERROR::INT(x1)::NAME(x2)::s1)::stl) (cmdtl::cmdtltl) env
      )
  |((NAME x1::INT x2::s1)::stl),((SUB::cmdtl)::cmdtltl),(env)-> 
        (let ny=check_name env (NAME(x1)) in 
        match x2,ny with
          |x2,Some(INT(y1))->processor((INT(x2-y1)::s1)::stl) (cmdtl::cmdtltl)  env
          |_->processor((ERROR::NAME(x1)::INT(x2)::s1)::stl) (cmdtl::cmdtltl)  env
        )
  |((s1)::stl),((SUB::cmdtl)::cmdtltl) ,env->processor((ERROR::s1)::stl) (cmdtl::cmdtltl) env

  |((INT x1::INT x2::s1)::stl),((MUL::cmdtl)::cmdtltl),env->processor((INT(x2*x1)::s1)::stl) (cmdtl::cmdtltl) env
  |((NAME x1::NAME x2::s1)::stl),((MUL::cmdtl)::cmdtltl),(env)->
    (let nx=check_name env (NAME(x1)) in 
    let ny=check_name env (NAME(x2)) in 
    match nx,ny with
      |Some(INT(x1)),Some (INT(y1))->processor((INT(y1*x1)::s1)::stl) (cmdtl::cmdtltl) env
      |_->processor((ERROR::NAME x1::NAME x2::s1)::stl) (cmdtl::cmdtltl) env
    )
    |((INT x1::NAME x2::s1)::stl),((MUL::cmdtl)::cmdtltl),(env)-> 
      (let ny=check_name env (NAME(x2)) in 
      match x1,ny with
        |x1,Some(INT(y1))->processor((INT(x1*y1)::s1)::stl) (cmdtl::cmdtltl) env
        |_->processor((ERROR::INT(x1)::NAME(x2)::s1)::stl) (cmdtl::cmdtltl) env
      )
    |((NAME x1::INT x2::s1)::stl),((MUL::cmdtl)::cmdtltl),(env)-> 
        (let ny=check_name env (NAME(x1)) in 
        match x2,ny with
          |x2,Some(INT(y1))->processor((INT(x2*y1)::s1)::stl) (cmdtl::cmdtltl) env
          |_->processor((ERROR::NAME(x1)::INT(x2)::s1)::stl) (cmdtl::cmdtltl) env
        )
  |((s1)::stl),((MUL::cmdtl)::cmdtltl),env->processor((ERROR::s1)::stl) (cmdtl::cmdtltl) env

  |((INT 0::INT x::s1)::stl),((DIV::cmdtl)::cmdtltl),env->processor((ERROR::INT 0::INT x::s1)::stl) (cmdtl::cmdtltl) env
  |((INT x1::INT x2::s1)::stl),((DIV::cmdtl)::cmdtltl),env->processor((INT(x2/x1)::s1)::stl) (cmdtl::cmdtltl) env
  |((NAME x1::NAME x2::s1)::stl),((DIV::cmdtl)::cmdtltl),(env)->
    (let nx=check_name env (NAME(x1)) in 
    let ny=check_name env (NAME(x2)) in 
    match nx,ny with
      |Some INT(0),Some INT(y)->processor((ERROR::NAME(x1)::NAME x2::s1)::stl) (cmdtl::cmdtltl) env
      |Some INT(x),Some INT(y)->processor((INT(y/x)::s1)::stl) (cmdtl::cmdtltl) env
      |_->processor((ERROR::NAME x1::NAME x2::s1)::stl) (cmdtl::cmdtltl) env
    )
    |((INT x1::NAME x2::s1)::stl),((DIV::cmdtl)::cmdtltl),(env)-> 
      (let ny=check_name env (NAME(x2)) in 
      match x1,ny with
        |0,Some INT(y)->processor((ERROR::INT(x1)::NAME x2::s1)::stl) (cmdtl::cmdtltl) env
        |x1,Some(INT(y1))->processor((INT(y1/x1)::s1)::stl) (cmdtl::cmdtltl) env
        |_->processor((ERROR::INT(x1)::NAME(x2)::s1)::stl) (cmdtl::cmdtltl) env
      )
    |((NAME x1::INT x2::s1)::stl),((DIV::cmdtl)::cmdtltl),(env)-> 
        (let ny=check_name env (NAME(x1)) in 
        match x2,ny with
          |x2,Some INT(0)->processor((ERROR::NAME(x1)::INT x2::s1)::stl) (cmdtl::cmdtltl) env
          |x2,Some(INT(y1))->processor((INT(x2/y1)::s1)::stl) (cmdtl::cmdtltl) env
          |_->processor((ERROR::NAME(x1)::INT(x2)::s1)::stl) (cmdtl::cmdtltl) env
        )
  |((s1)::stl),((DIV::cmdtl)::cmdtltl),env->processor((ERROR::s1)::stl) (cmdtl::cmdtltl) env

  |((INT 0::INT x::s1)::stl),((REM::cmdtl)::cmdtltl),env->processor((ERROR::INT 0::INT x::s1)::stl) (cmdtl::cmdtltl) env
  |((INT x1::INT x2::s1)::stl),((REM::cmdtl)::cmdtltl),env->processor((INT(x2 mod x1)::s1)::stl) (cmdtl::cmdtltl) env
  |((NAME x1::NAME x2::s1)::stl),((REM::cmdtl)::cmdtltl),(env)->
    (let nx=check_name env (NAME(x1)) in 
    let ny=check_name env (NAME(x2)) in 
    match nx,ny with
      |Some INT(0),Some INT _->processor((ERROR::NAME x1::NAME x2::s1)::stl) (cmdtl::cmdtltl) env
      |Some INT(x1),Some INT(y1)->processor((INT(y1 mod x1)::s1)::stl) (cmdtl::cmdtltl) env
      |_->processor((ERROR::NAME x1::NAME x2::s1)::stl) (cmdtl::cmdtltl) env
    )
    |((INT x1::NAME x2::s1)::stl),((REM::cmdtl)::cmdtltl),(env)-> 
      (let ny=check_name env (NAME(x2)) in 
      match x1,ny with
        |0,Some INT(y)->processor((ERROR::INT(x1)::NAME x2::s1)::stl) (cmdtl::cmdtltl) env
        |x1,Some(INT(y1))->processor((INT(y1 mod x1)::s1)::stl) (cmdtl::cmdtltl) env
        |_->processor((ERROR::INT(x1)::NAME(x2)::s1)::stl) (cmdtl::cmdtltl) env
      )
    |((NAME x1::INT x2::s1)::stl),((REM::cmdtl)::cmdtltl),(env)-> 
        (let ny=check_name env (NAME(x1)) in 
        match x2,ny with
          |x2,Some INT(0)->processor((ERROR::NAME(x1)::INT x2::s1)::stl) (cmdtl::cmdtltl) env
          |x2,Some(INT(y1))->processor((INT(x2 mod y1)::s1)::stl) (cmdtl::cmdtltl) env
          |_->processor((ERROR::NAME(x1)::INT(x2)::s1)::stl) (cmdtl::cmdtltl) env
        )
    |((s1)::stl),((REM::cmdtl)::cmdtltl),env->processor((ERROR::s1)::stl) (cmdtl::cmdtltl) env
  
  |((INT x1::s1)::stl),((NEG::cmdtl)::cmdtltl),env->processor((INT(-x1)::s1)::stl) (cmdtl::cmdtltl) env
  |((NAME x1::s1)::stl),((NEG::cmdtl)::cmdtltl),env-> 
    (let ny=check_name env (NAME(x1)) in 
    match ny with
      |Some(INT(y1))->processor((INT(-y1)::s1)::stl) (cmdtl::cmdtltl) env
      |_->processor((ERROR::NAME(x1)::s1)::stl) (cmdtl::cmdtltl) env
    )
    |((s1)::stl),((NEG::cmdtl)::cmdtltl),env->processor((ERROR::s1)::stl) (cmdtl::cmdtltl) env

  |((x1::x2::s1)::stl),((SWAP::cmdtl)::cmdtltl),env->processor((x2::x1::s1)::stl) (cmdtl::cmdtltl) env
  |((s1)::stl),((SWAP::cmdtl)::cmdtltl),env->processor((ERROR::s1)::stl) (cmdtl::cmdtltl) env
  
  |((x1::s1)::stl),((TO_STRING::cmdtl)::cmdtltl),env->processor((STRING(sv2str(x1))::s1)::stl) (cmdtl::cmdtltl) env
  |(s1::stl),((TO_STRING::cmdtl)::cmdtltl),env->processor((ERROR::s1)::stl) (cmdtl::cmdtltl) env

  |((x1::s1)::stl),((PRINTLN::cmdtl)::cmdtltl),env->(
    match x1 with
    |STRING(x1)->Printf.fprintf oc "%s\n" x1;processor (s1::stl) (cmdtl::cmdtltl) env
    |_->processor ((ERROR::s1)::stl) (cmdtl::cmdtltl) env
  )
  |(stl),((QUIT::cmdtl)::cmdtltl),env->stl,env
  
  (*part2*)
  |((STRING(x1)::STRING(x2)::s1)::stl),((CAT::cmdtl)::cmdtltl),env->processor((STRING(x2^x1)::s1)::stl) (cmdtl::cmdtltl) env
  |((NAME x1::NAME x2::s1)::stl),((CAT::cmdtl)::cmdtltl),(env)->
    (let nx=check_name env (NAME(x1)) in 
    let ny=check_name env (NAME(x2)) in 
    match nx,ny with
      |Some(STRING(x1)),Some(STRING(y1))->processor((STRING(y1^x1)::s1)::stl) (cmdtl::cmdtltl) env
      |_->processor((ERROR::NAME x1::NAME x2::s1)::stl) (cmdtl::cmdtltl) env
    )
  |((STRING x1::NAME x2::s1)::stl),((CAT::cmdtl)::cmdtltl),(env)-> 
      (let ny=check_name env (NAME(x2)) in 
      match x1,ny with
        |x1,Some(STRING(y1))->processor((STRING(y1^x1)::s1)::stl) (cmdtl::cmdtltl) env
        |_->processor((ERROR::STRING(x1)::NAME(x2)::s1)::stl) (cmdtl::cmdtltl) env
      )
  |((NAME x1::STRING x2::s1)::stl),((CAT::cmdtl)::cmdtltl),(env)-> 
        (let ny=check_name env (NAME(x1)) in 
        match x2,ny with
          |x2,Some(STRING(y1))->processor((STRING(x2^y1)::s1)::stl) (cmdtl::cmdtltl) env
          |_->processor((ERROR::NAME(x1)::STRING(x2)::s1)::stl) (cmdtl::cmdtltl) env
        )
  |((s1)::stl),((CAT::cmdtl)::cmdtltl),env->processor((ERROR::s1)::stl) (cmdtl::cmdtltl) env

  |((BOOL(x1)::BOOL(x2)::s1)::stl),((AND::cmdtl)::cmdtltl),env->processor((BOOL(x2&&x1)::s1)::stl) (cmdtl::cmdtltl) env
  |((NAME x1::NAME x2::s1)::stl),((AND::cmdtl)::cmdtltl),(env)->
    (let nx=check_name env (NAME(x1)) in 
    let ny=check_name env (NAME(x2)) in 
    match nx,ny with
      |Some(BOOL(x1)),Some(BOOL(y1))->processor((BOOL(y1&&x1)::s1)::stl) (cmdtl::cmdtltl) env
      |_->processor((ERROR::NAME x1::NAME x2::s1)::stl) (cmdtl::cmdtltl) env
    )
  |((BOOL x1::NAME x2::s1)::stl),((AND::cmdtl)::cmdtltl),(env)-> 
      (let ny=check_name env (NAME(x2)) in 
      match x1,ny with
        |x1,Some(BOOL(y1))->processor((BOOL(y1&&x1)::s1)::stl) (cmdtl::cmdtltl) env
        |_->processor((ERROR::BOOL(x1)::NAME(x2)::s1)::stl) (cmdtl::cmdtltl) env
      )
  |((NAME x1::BOOL x2::s1)::stl),((AND::cmdtl)::cmdtltl),(env)-> 
        (let ny=check_name env (NAME(x1)) in 
        match x2,ny with
          |x2,Some(BOOL(y1))->processor((BOOL(x2&&y1)::s1)::stl) (cmdtl::cmdtltl) env
          |_->processor((ERROR::NAME(x1)::BOOL(x2)::s1)::stl) (cmdtl::cmdtltl) env
        )
  |((s1)::stl),((AND::cmdtl)::cmdtltl),env->processor((ERROR::s1)::stl) (cmdtl::cmdtltl) env

  |((BOOL(x1)::BOOL(x2)::s1)::stl),((OR::cmdtl)::cmdtltl),env->processor((BOOL(x2||x1)::s1)::stl) (cmdtl::cmdtltl) env
  |((NAME x1::NAME x2::s1)::stl),((OR::cmdtl)::cmdtltl),(env)->
    (let nx=check_name env (NAME(x1)) in 
    let ny=check_name env (NAME(x2)) in 
    match nx,ny with
      |Some(BOOL(x1)),Some(BOOL(y1))->processor((BOOL(y1||x1)::s1)::stl) (cmdtl::cmdtltl) env
      |_->processor((ERROR::NAME x1::NAME x2::s1)::stl) (cmdtl::cmdtltl) env
    )
  |((BOOL x1::NAME x2::s1)::stl),((OR::cmdtl)::cmdtltl),(env)-> 
      (let ny=check_name env (NAME(x2)) in 
      match x1,ny with
        |x1,Some(BOOL(y1))->processor((BOOL(y1||x1)::s1)::stl) (cmdtl::cmdtltl) env
        |_->processor((ERROR::BOOL(x1)::NAME(x2)::s1)::stl) (cmdtl::cmdtltl) env
      )
  |((NAME x1::BOOL x2::s1)::stl),((OR::cmdtl)::cmdtltl),(env)-> 
        (let ny=check_name env (NAME(x1)) in 
        match x2,ny with
          |x2,Some(BOOL(y1))->processor((BOOL(x2||y1)::s1)::stl) (cmdtl::cmdtltl) env
          |_->processor((ERROR::NAME(x1)::BOOL(x2)::s1)::stl) (cmdtl::cmdtltl) env
        )
  |((s1)::stl),((OR::cmdtl)::cmdtltl),env->processor((ERROR::s1)::stl) (cmdtl::cmdtltl) env

  |((BOOL(x1)::s1)::stl),((NOT::cmdtl)::cmdtltl),env->processor((BOOL(not x1)::s1)::stl) (cmdtl::cmdtltl) env
  |((NAME x1::s1)::stl),((NOT::cmdtl)::cmdtltl),env-> 
    (let ny=check_name env (NAME(x1)) in 
    match ny with
      |Some(BOOL(y1))->processor((BOOL(not y1)::s1)::stl) (cmdtl::cmdtltl) env
      |_->processor((ERROR::NAME(x1)::s1)::stl) (cmdtl::cmdtltl) env
    )
  |((s1)::stl),((NOT::cmdtl)::cmdtltl),env->processor((ERROR::s1)::stl) (cmdtl::cmdtltl) env
  
  
  |((INT(x1)::INT(x2)::s1)::stl),((EQUAL::cmdtl)::cmdtltl),env->processor((BOOL(x2=x1)::s1)::stl) (cmdtl::cmdtltl) env
  |((INT x1::NAME x2::s1)::stl),((EQUAL::cmdtl)::cmdtltl),(env)-> 
    (let ny=check_name env (NAME(x2)) in 
    match x1,ny with
      |x1,Some(INT(y1))->processor((BOOL(x1=y1)::s1)::stl) (cmdtl::cmdtltl) env
      |_->processor((ERROR::INT(x1)::NAME(x2)::s1)::stl) (cmdtl::cmdtltl) env
    )
  |((NAME x1::INT x2::s1)::stl),((EQUAL::cmdtl)::cmdtltl),(env)-> 
      (let ny=check_name env (NAME(x1)) in 
      match x2,ny with
        |x2,Some(INT(y1))->processor((BOOL(x2=y1)::s1)::stl) (cmdtl::cmdtltl) env
        |_->processor((ERROR::NAME(x1)::INT(x2)::s1)::stl) (cmdtl::cmdtltl) env
      )
  |((NAME x1::NAME x2::s1)::stl),((EQUAL::cmdtl)::cmdtltl),(env)->
    (let nx=check_name env (NAME(x1)) in 
    let ny=check_name env (NAME(x2)) in 
    match nx,ny with
      |Some(INT(x1)),Some(INT(y1))->processor((BOOL(x1=y1)::s1)::stl) (cmdtl::cmdtltl) env
      |_->processor((ERROR::NAME x1::NAME x2::s1)::stl) (cmdtl::cmdtltl) env
    )
  |((s1)::stl),((EQUAL::cmdtl)::cmdtltl),env->processor((ERROR::s1)::stl) (cmdtl::cmdtltl) env
  
  |((INT(x1)::INT(x2)::s1)::stl),((LESS_THAN::cmdtl)::cmdtltl),env->processor((BOOL(x2<x1)::s1)::stl) (cmdtl::cmdtltl) env
  |((INT x1::NAME x2::s1)::stl),((LESS_THAN::cmdtl)::cmdtltl),(env)-> 
    (let ny=check_name env (NAME(x2)) in 
    match x1,ny with
      |x1,Some(INT(y1))->processor((BOOL(y1<x1)::s1)::stl) (cmdtl::cmdtltl) env
      |_->processor((ERROR::INT(x1)::NAME(x2)::s1)::stl) (cmdtl::cmdtltl) env
    )
  |((NAME x1::INT x2::s1)::stl),((LESS_THAN::cmdtl)::cmdtltl),(env)-> 
      (let ny=check_name env (NAME(x1)) in 
      match x2,ny with
        |x2,Some(INT(y1))->processor((BOOL(x2<y1)::s1)::stl) (cmdtl::cmdtltl) env
        |_->processor((ERROR::NAME(x1)::INT(x2)::s1)::stl) (cmdtl::cmdtltl) env
      )
  |((NAME x1::NAME x2::s1)::stl),((LESS_THAN::cmdtl)::cmdtltl),(env)->
    (let nx=check_name env (NAME(x1)) in 
    let ny=check_name env (NAME(x2)) in 
    match nx,ny with
      |Some(INT(x1)),Some(INT(y1))->processor((BOOL(y1<x1)::s1)::stl) (cmdtl::cmdtltl) env
      |_->processor((ERROR::NAME x1::NAME x2::s1)::stl) (cmdtl::cmdtltl) env
    )
  |((s1)::stl),((LESS_THAN::cmdtl)::cmdtltl),env->processor((ERROR::s1)::stl) (cmdtl::cmdtltl) env

  |((s1)::stl),((BIND::cmdtl)::cmdtltl),(e1::env)->(
    match s1 with
    | v::NAME(n)::tail ->(
      match v with
      |NAME(x)->(
        let nx=check_name (e1::env) (NAME(x)) in  
    match nx with
      |Some x->processor((UNIT::tail)::stl) (cmdtl::cmdtltl) (((NAME(n),x)::e1)::env)
      |None->processor(((ERROR::NAME(x)::NAME(n)::tail)::stl)) (cmdtl::cmdtltl) (e1::env)
      )
    |ERROR->processor(((ERROR::ERROR::NAME(n)::tail)::stl)) (cmdtl::cmdtltl) (e1::env)
    |_->processor((UNIT::tail)::stl) (cmdtl::cmdtltl) (((NAME(n),v)::e1)::env)
      )
    |_->processor((ERROR::s1)::stl) (cmdtl::cmdtltl) (e1::env)
    ) 
  
  |((a::b::BOOL(c)::s1)::stl),((IF::cmdtl)::cmdtltl),env->(
  match c with
  |  true->processor((a::s1)::stl) (cmdtl::cmdtltl) env
  | false->processor((b::s1)::stl) (cmdtl::cmdtltl) env
  )
  |((a::b::NAME(c)::s1)::stl),((IF::cmdtl)::cmdtltl),env->(
    let nc=check_name env (NAME(c)) in
  match nc with
  |  Some(BOOL(true))->processor((a::s1)::stl) (cmdtl::cmdtltl) env
  | Some(BOOL(false))->processor((b::s1)::stl) (cmdtl::cmdtltl) env
  |_->processor((ERROR::a::b::NAME(c)::s1)::stl) (cmdtl::cmdtltl) env
  )
  |((s1)::stl),((IF::cmdtl)::cmdtltl),env->processor((ERROR::s1)::stl) (cmdtl::cmdtltl) env

  |(stack,((LET::cmdtl)::cmdtltl),env)->processor([]::stack) (cmdtl::cmdtltl) ([]::env)
  |((s1::s2::s3),((END::cmdtl)::cmdtltl),e1::e2)->(*s2=original stack*)
    (match s1,s2,s3 with 
    |((sv::stl),s2,s3)->processor ((sv::s2)::s3) (cmdtl::cmdtltl) e2
    |_->processor (s2::s3) (cmdtl::cmdtltl) e2)
  
(*part3*)
  |(s1::stl),((FUN(funname,arg)::cmdtl)::cmdtltl),(e1::e2)->
    (
      match (checkFun cmdtl [] 0) with
      |(fragment,cmds)->processor ((UNIT::s1)::stack) (cmds::cmdtltl) (((funname,CLOSURE(true,arg,fragment,env))::e1)::e2)
    )

  |(s1::stl),((IN_OUT_FUN(fname,arg)::cmd)::cmdtl),(e1::e2)->(
    match (checkFun cmd [] 0) with
    |(fragment,cmds)->processor ((UNIT::s1)::stack) (cmds::cmdtl) (((fname,CLOSURE(false,arg,fragment,env))::e1)::e2)
  )

  |((NAME(n)::funname::s1)::stl),((CALL::cmd)::cmdtl),(en1::en2 as env)->(
    match (check_name env (NAME(n))) ,(check_name env (funname)) with
    |Some argue, Some CLOSURE(b,args,fragment,e1::e2)->
      if b = true then 
      (let (s,_)=processor ([]::s1::stl) (fragment::cmd::cmdtl) (((args,argue)::e1)::e2) in 
      (
        match s with
        | ((hd::tl)::tail) -> processor ((hd::s1)::stl) (cmd::cmdtl) (env)
        |([]::tl)->processor (s1::stl) (cmd::cmdtl) (env)
        |_->processor ((ERROR::s1)::stl) (cmd::cmdtl) env
      ))
else(
  let (s,e)=processor ([]::s1::stl) (fragment::cmd::cmdtl) (((args,argue)::en1)::en2) in 
    (
      match s,argue with
      | ((hd::tl)::tail),NAME(argss) ->
        let transmit= check_name e args in (
          match argue, transmit with 
          |NAME n, Some sv-> processor ((hd::s1)::stl) (cmd::cmdtl) ((en1)::([(NAME n,argue)]::en2))
          |_,_->processor ((hd::s1)::stl) (cmd::cmdtl) ((en1)::([(NAME n,argue)]::en2))
        )
      |_->processor ((ERROR::s1)::stl) (cmd::cmdtl) env
    )
)
  |_->processor ((ERROR::NAME(n)::funname::s1)::stl) (cmd::cmdtl) env
  )

  |((argue::NAME funname::s1)::stl),((CALL::cmd)::cmdtl),(en1::en2 as env)->(
    match (check_name env (NAME funname)) with
    |Some CLOSURE(b,args,fragment,e1::e2)->
      let newstack=[]::s1::stl in 
      let newenv=((args,argue)::e1)::e2 in 
      if b=true then(
      let (s,_)=processor newstack [fragment@[RETURN]] newenv in 
      (
        match s with
        | ((hd::tl)::tail) ->
          processor ((hd::s1)::stl) (cmd::cmdtl) env
        |([]::tl)->processor (s1::stl) (cmd::cmdtl) env
        |_->processor ((ERROR::s1)::stl) (cmd::cmdtl) env
      ))
else(
  let (s,_)= processor newstack [fragment@[RETURN]] (newenv@env) in 
      (
        match s with
        | ((hd::tl)::tail) ->processor  ((hd::s1)::stl) (cmd::cmdtl) (newenv@env)
        |_->processor ((ERROR::s1)::stl) (cmd::cmdtl) env
      )
)
    |_->processor ((ERROR::argue::NAME funname::s1)::stl) (cmd::cmdtl) (env)
  )

  |stl,((RETURN::cmd)::cmdtl),env->stl,env

  |(s1::stl),((FUN_END::cmd)::cmdtl),env->processor ((ERROR::s1)::stl) (cmd::cmdtl) env



|_,_,_->stack,env
in
let _ =processor [[]] [commands] [[]]
in ()
;;
(*interpreter("input4.txt","output100.txt")*)
(*
cannot use in part2
let rec processor (stack:stackValue list)(cmd:command list)(env:part2)=
match cmd with
| []->stack 
|cmd::tl->
  let sk,en=
  match cmd with
  |PUSH a->a::stack,env
  |POP->(
    match stack with
    | [] -> ERROR::stack
    |_::tal->tal),env

  |ADD->(match stack with
  | INT x::INT y::tl -> INT(x+y)::tl,env
  |NAME x::NAME y::tl->
    (let x1=check_name env x in 
    let y1=check_name env y in 
    match x1,y1 with
      |INT(x1),INT(y1)->(INT(x1+y1)::tl,env)
      |_->ERROR::stack,env
    )
  |_->ERROR::stack,env)

  |SUB->(match stack with
  | INT x :: INT y::tl -> INT(y-x)::tl,env
  |NAME x::NAME y::tl->
    (let x1=check_name env x in 
    let y1=check_name env y in 
    match x1,y1 with
      |INT(x1),INT(y1)->(INT(x1-y1)::tl,env)
      |_->ERROR::stack,env
    )
  |_->ERROR::stack,env)

  |MUL->(match stack with
  |INT x::INT y::tk->INT(x*y)::tk,env
  |NAME x::NAME y::tl->
    (let x1=check_name env x in 
    let y1=check_name env y in 
    match x1,y1 with
      |INT(x1),INT(y1)->(INT(x1*y1)::tl,env)
      |_->ERROR::stack,env
    )
  |_->ERROR::stack,env)

  |DIV->(match stack with
  | INT _::INT 0::tl ->ERROR::tl,env
  |INT x::INT y::tl->INT(y/x)::tl,env
  |NAME x::NAME y::tl->
    (let x1=check_name env x in 
    let y1=check_name env y in 
    match x1,y1 with
      |INT(x1),INT(0)->ERROR::tl,env
      |INT(x1),INT(y1)->(INT(y1/x1)::tl,env)
      |_->ERROR::stack,env
    )
  |_->ERROR::stack,env)

  |REM->(match stack with
  | INT _::INT 0::tl -> ERROR::tl,env
  |NAME x::NAME y::tl->
    (let x1=check_name env x in 
    let y1=check_name env y in 
    match x1,y1 with
      |INT(x1),INT(0)->ERROR::tl,env
      |INT(x1),INT(y1)->(INT(y1 mod x1)::tl,env)
      |_->ERROR::stack,env
    )
  |INT x::INT y::tl->INT(y mod x)::tl,env
  |_->ERROR::stack,env)

  |NEG->(match stack with
  | INT x::tl -> INT(-x)::tl
  |_->ERROR::stack),env

  |SWAP->(match stack with
  | x::y::tl -> y::x::tl
  |_->ERROR::stack),env

  |TO_STRING->(match stack with
  | [] -> ERROR::stack
  |INT n::tl->STRING(string_of_int n)::tl
  |BOOL true::tl->STRING ":true:" ::tl
  |BOOL false::tl->STRING":false:"::tl
  |ERROR::tl->STRING":error:"::tl
  |UNIT::tl->STRING":unit:"::tl
  |STRING s::tl->STRING s::tl
  |NAME s::tl->STRING s::tl),env
  |PRINTLN->(match stack with
  |STRING s::tl-> Printf.fprintf oc "%s\n" s;tl
  |_->ERROR::stack),env
  |QUIT->stack,env
  (*part2*)
  |CAT->(
    match stack with
    |STRING s1::STRING s2::tl->STRING(s2^s1)::tl
    |_->ERROR::stack
    ),env
  |AND->(
    match stack with 
    |BOOL b1::BOOL b2::tl->BOOL(b2&&b1)::tl
    |_->ERROR::stack
  ),env
  |OR->(
    match stack with 
    |BOOL b1::BOOL b2::tl->BOOL(b2||b1)::tl
    |_->ERROR::stack
  ),env
  |NOT->(
    match stack with 
    |BOOL b1::tl->BOOL(not b1)::tl
    |_->ERROR::stack
  ),env
  |EQUAL->(
    match stack with 
    |INT x::INT y::tl->BOOL(y=x)::tl
    |_->ERROR::stack
  ),env
  |LESS_THAN->(
    match stack with 
    |INT x::INT y::tl->BOOL(y<x)::tl,env
    |_->ERROR::stack,env
  )
  |BIND->(
    match stack with 
  |NAME n1::NAME n2::tl->check_name env n1::tl,env
  |ERROR::NAME n :: tl->ERROR::tl,env
  |x::NAME n::tl->UNIT::tl,check env n x
  |_->ERROR::stack,env)

  |IF->(
    match stack with
    |x::y::BOOL true:: tl->x::tl,env
    |x::y::BOOL false::tl->y::tl,env
    |_->ERROR::stack,env 
  )
  |LET->(

    let rec newscope cmd layer arr =
      match cmd with
      | [] -> List.rev arr
      |END::tail when layer=0->List.rev arr
      |LET::tail->newscope tail (layer+1) (LET::arr)
      |END::tail->newscope tail (layer-1)(END::arr)
      |head::tail->newscope tail layer (head::arr)(*add all commands into arr and let arr be the new commands*)
    in 
    let scope_cmd=newscope tl 0 [] in 
    
    let scope_stack=processor [] scope_cmd env in 
    match scope_stack with 
    |hd::_ ->(hd::stack),env
    |_->(ERROR::stack),env
  )
  
  |END->stack,env
  
in
processor sk tl en
in*)