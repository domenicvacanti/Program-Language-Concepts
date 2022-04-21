{
module Grammar where
import Tokens
import Tp
}

%name parseTp
%tokentype { Token }
%error { parseError }

%token
    '('  { TokenLParen }
    ')'  { TokenRParen }
    '->'  { TokenArrow}

%right '->'

%%

-- replace this with your productions:
Tp
: Tp '->' Tp { Arrow $1 $3 }
| '(' Tp ')' { $2 }
| '('')' { Unit }

{

parseError :: [Token] -> a
parseError tks = error ("Parse error: " ++ show tks)

}
