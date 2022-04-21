{
module Tokens where
}

%wrapper "basic"

tokens :-

  ab*                         { TokenIt "p0" }
  [cd]+                       { TokenIt "p1" }
  [ef]+                       { TokenIt "p2" }
  [a-z]+[0-9]+                { TokenIt "p3" }
  [\"\'][a-zA-Z\ ]+[\"\']     { TokenIt "p4" }
  ((Y*XY*){2})+|(Y+)          { TokenIt "p5" }

  -- add further regular expressions here:
  
  $white+                       ;
{
-- The token type:
data Token = TokenIt String {- which problem the string is for -} String
           deriving (Eq)

instance Show Token where
  show (TokenIt name str) = name ++ "(" ++ show str ++ ")"
}
