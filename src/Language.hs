module Language where

import           Data.Char (isAlpha, isDigit)
data Expr a =
  Var Name
  | Num Int
  | Constr Int Int
  | Ap (Expr a) (Expr a)
  | Let IsRec [(a, Expr a)] (Expr a)
  | Case (Expr a) [Alter a]
  | Lam [a] (Expr a)
  deriving (Show)

type CoreExpr = Expr Name
type Name = String

type IsRec = Bool

recursive :: IsRec
recursive = True

nonRecursive :: IsRec
nonRecursive = False

bindersOf :: [(a,b)] -> [a]
bindersOf defns = [name | (name, _) <- defns]

rhssOf :: [(a,b)] -> [b]
rhssOf defns = [rhs | (_, rhs) <- defns]

type Alter a = (Int, [a], Expr a)
type CoreAlter = Alter Name

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (Var _) = True
isAtomicExpr (Num _) = True
isAtomicExpr _       = False

type Program a = [ScDefn a]
type CoreProgram = Program Name

type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

example1 =
  [ ("main", [], Ap (Var "double") (Num 21))
  , ("double", ["x"], Ap (Ap (Var "+") (Var "x")) (Var "x"))
  , ("f", [], Let False [("a", Num 2), ("b", Num 3)] (Var "a"))
  , ("g", ["x"], Case (Var "x") [(0, [], Num 100), (1, [], Num 500)])
  ]

prelude =
  [ ("I", ["x"], Var "x")
  , ("K", ["x", "y"], Var "x")
  , ("K1", ["x", "y"], Var "y")
  , ("S", ["f", "g", "x"], Ap (Ap (Var "f") (Var "x")) (Ap (Var "g") (Var "x")))
  , ("compose", ["f", "g", "x"], Ap (Var "f") (Ap (Var "g") (Var "x")))
  , ("twice", ["f"], Ap (Ap (Var "compose") (Var "f")) (Var "f"))
  ]


pprint :: CoreProgram -> String
pprint prog = iDisplay $ iInterleave Newline (map pprScDfn prog)
  where
    pprScDfn :: CoreScDefn -> IseqRep
    pprScDfn (n, as, expr) = str n <+> str " " <+> iInterleave (str " ") (map str as) <+> str " = " <+> pprExpr expr

    pprExpr :: Expr Name -> IseqRep
    pprExpr (Num n)    = str $ show n
    pprExpr (Var v)    = str v
    pprExpr (Constr tag arty) = str "Pack{" <+> str (show tag) <+> str ","
      <+> str (show arty) <+> str "}"
    pprExpr (Ap e1 e2) = pprExpr e1 `append` str " " `append` pprExpr e2
    pprExpr (Let isrec defns expr) = iConcat
      [ str keyword, newline
      , str " ", indent (pprDefns defns), newline
      , str "in ", pprExpr expr
      ]
      where
        keyword | not isrec = "let"
                | isrec = "letrec"
    pprExpr (Case expr alts) = str "case " <+> pprExpr expr <+> str " of" <+> newline
      <+> str "  " <+> indent (iInterleave nl (map pprAlt alts))
      where
        nl = str ";" `append` newline
        pprAlt (t, as, expr) = str "<" <+> str (show t) <+> str ">" <+>
          iInterleave (str " ") (map str as) <+> str " -> " <+> pprExpr expr

    pprExpr (Lam as expr) = str "\\" <+> iInterleave (str " ") (map str as) <+> str "->" <+> pprExpr expr

    pprAExpr e
      | isAtomicExpr e = pprExpr e
      | otherwise = str "(" `append` pprExpr e `append` str ")"

    pprDefns :: [(Name, CoreExpr)] -> IseqRep
    pprDefns defns = iInterleave sep (map pprDefn defns)
      where sep = iConcat [str ";", newline]

    pprDefn :: (Name, CoreExpr) -> IseqRep
    pprDefn (name, expr) = iConcat [str name, str " = ", indent (pprExpr expr)]

mkMultiAp n e1 e2 = foldl Ap e1 $ replicate n e2

class Iseq a where
  nil :: a
  str :: String -> a
  append :: a -> a -> a
  newline :: a
  indent :: a -> a
  display :: a -> String

(<+>) = append

iConcat :: Iseq a => [a] -> a
iConcat = foldr append nil

iInterleave :: Iseq a => a -> [a] -> a
iInterleave _ []     = nil
iInterleave _ [x]    = x
iInterleave s (x:xs) = x `append` s `append` iInterleave s xs

infixr 5 `append`
infixr 5 <+>

data IseqRep =
  Nil
  | Str String
  | Append IseqRep IseqRep
  | Indent IseqRep
  | Newline

instance Iseq IseqRep where
  indent = Indent
  newline = Newline
  append e1 e2 = Append e1 e2
  str s = Str s
  nil = Nil

flatten :: Int -> [(IseqRep, Int)] -> String
flatten col []                         = ""
flatten col ((Nil, _):xs)              = flatten col xs
flatten col ((Str s, _):xs)            = s <> flatten (col + length s) xs
flatten col ((Append seq1 seq2, i):xs) = flatten col ((seq1, i):(seq2, i):xs)
flatten col ((Newline, i):xs)          = "\n" <> replicate i ' ' <> flatten 0 xs
flatten col ((Indent e, i):xs)         = flatten col ((e, col):xs)

iDisplay :: IseqRep -> String
iDisplay seq = flatten 0 [(seq, 0)]

space n = replicate n ' '

iNum n = Str (show n)

iFNum width n = Str $ space (width - length digits) ++ digits
  where digits = show n

iLayn seqs = iConcat (zipWith (curry layItem) [1..] seqs)
  where layItem (n, seq) = iConcat [iFNum 4 n, str ")", Indent seq, Newline]

test = do
  putStrLn "-- example"
  putStrLn $ pprint example1
  putStrLn "-- plulude"
  putStrLn $ pprint prelude



parse :: String -> CoreProgram
parse = syntax . clex 0

type Token = (Int, String)

opsStrs = ["==", "/=", ">=", "<=", "->"]

clex :: Int -> String -> [Token]
clex n ('-':'-':cs) =
  let (_, rest) = span (/= '\n') cs
  in clex n rest
clex n (c:c':cs)
  | [c,c'] `elem` opsStrs = (n, [c,c']) : clex n cs
clex n (c:cs)
  | c == '\n' = clex (n+1) cs
  | isWhiteSpace c = clex n cs
  | isDigit c =
      let (numCs, restCs) = span isDigit cs
          numToken = c : numCs
      in (n, numToken) : clex n restCs
  | isAlpha c =
      let (idCs, restCs) = span isIdChar cs
          varToken = c : idCs
      in (n, varToken) : clex n restCs
clex n (c:cs) =
  (n, [c]) : clex n cs
clex n [] = []

isWhiteSpace c = c `elem` " \t\n"
isIdChar c = isAlpha c || isDigit c || c == '_'

type Parser a = [Token] -> [(a, [Token])]

-- pLit :: String -> Parser String
-- pLit s (t:ts)
--   | s == snd t = [(s, ts)]
--   | otherwise = []
-- pLit s [] = []

-- pVar :: Parser String
-- pVar ((_, t):ts) | isAlpha (head t) = [(t, ts)] -- todo: check keyword
-- pVar []          = []

pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = p1 toks ++ p2 toks

infixr 4 `pAlt`

pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen c p1 p2 toks =
  [ (c v1 v2, t2) | (v1, t1) <- p1 toks
                  , (v2, t2) <- p2 t1 ]

pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 c p1 p2 p3 toks =
  [ (c v1 v2 v3, t') | (v1, t1) <- p1 toks
                     , (v2, t2) <- p2 t1
                     , (v3, t') <- p3 t2 ]


pThen4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
pThen4 c p1 p2 p3 p4 toks =
  [ (c v1 v2 v3 v4, t') | (v1, t1) <- p1 toks
                        , (v2, t2) <- p2 t1
                        , (v3, t3) <- p3 t2
                        , (v4, t') <- p4 t3 ]

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = pOneOrMore p `pAlt` pEmpty []

pEmpty :: a -> Parser a
pEmpty x toks = [(x, toks)]

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = pThen (:) p (pZeroOrMore p)

pApply :: Parser a -> (a -> b) -> Parser b
pApply p f toks = do
  [ (f v, toks') | (v, toks') <- p toks ]

(<$$>) = flip pApply

pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p sep =
  pThen (:) p $  pOneOrMore (sep `op` p)
  where op = pThen (\_ b -> b)
 -- (:) <$$> p <**> pZeroOrMore (sep **> p)

pap :: Parser (a -> b) -> Parser a -> Parser b
pap pf pa toks =
  [ (f x, t2) | (f, t1) <- pf toks
              , (x, t2) <- pa t1
              ]

(<**>) = pap

(<**) = pThen const
(**>) = pThen (\_ b -> b)

pSat :: (String -> Bool) -> Parser String
pSat pre ((_, t):ts) | pre t = [(t, ts)]
pSat _ _             = []

pLit :: (String -> Parser String)
pLit s = pSat (== s)

pVar = pSat (\t -> isAlpha (head t)  && (t `notElem` keywords))

keywords = ["let", "letrec", "in", "case", "of", "Pack"]

pNum :: Parser Int
pNum = read <$$> pSat (all isDigit)


syntax :: [Token] -> CoreProgram
syntax = takeFirstParse . pProgram
  where
    takeFirstParse ((prog, []): others) = prog
    takeFirstParse (parse : others)     = takeFirstParse others
    takeFirstParse others               = error "syntax error"

pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

pSc :: Parser CoreScDefn
pSc = pThen4 mkSc pVar (pZeroOrMore pVar) (pLit "=") pExpr

mkSc :: a -> b -> p -> c -> (a, b, c)
mkSc fun args _  expr = (fun , args, expr)

pExpr :: Parser CoreExpr
pExpr =
  pLet `pApply` pCase `pApply` pAexpr

pAexpr :: Parser CoreExpr
pAexpr =
  Var <$$> pVar
  `pAlt`
  Num <$$> pNum
  `pAlt`
  pPack
  `pAlt`
  pLit "(" **> pExpr <** pLit ")"

pPack :: Parser CoreExpr
pPack =
  Constr <$$> (pLit "Pack" **> pLit "{" **> pNum) <**> (pLit "," **> pNum <** pLit "}")

pLet = undefined
pCase = undefined
