{
module Grammar where
import Tokens
import Prog
}

%name parseProg
%tokentype { Token }
%error { parseError }

%token
    '('  { TokenLParen }
    ')'  { TokenRParen }
    ';'  { TokenSemi}
    ','  { TokenComma}
    id  { TokenId $$ }
%%

-- replace with your productions:
Prog
: FunCall ';'     { [$1] }
| FunCall ';'      Prog { $1 : $3 }

FunCall
: id        { Var $1 }
| id '(' Cons ')' { FunCall $1 $3 }
| id '(' ')'    { FunCall $1 [] }

Cons
: FunCall    { [$1] }
| FunCall ',' Cons { $1 : $3 }

{

parseError :: [Token] -> a
parseError tks = error ("Parse error: " ++ show tks)

}
