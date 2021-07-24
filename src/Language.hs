module Language where

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

test = do
  putStrLn "-- example"
  putStrLn $ pprint example1
  putStrLn "-- plulude"
  putStrLn $ pprint prelude
