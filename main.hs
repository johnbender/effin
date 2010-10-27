module Main where
import Language.C as C
import Language.C.Syntax.AST
import Language.C.Data.Ident
import Language.C.Analysis
import Language.C.Analysis.TravMonad
import Language.C.System.GCC
import Data.List
import Data.Maybe
import Data.Either
import Control.Monad
import Control.Monad.Writer
import System( getArgs )
import System.Console.GetOpt

main = do
  args <- getArgs
  let (flags, nonOpts, msgs) = getOpt RequireOrder options args
  getAnalysed $ head nonOpts

data Flag = Version

options :: [OptDescr Flag]
options = [ Option ['V'] ["version"] (NoArg Version) "show version number" ]

getAnalysed path = do parseResult  <- parseCFile (newGCC "gcc") Nothing [] path
                      case parseResult of
                        Right (CTranslUnit x _) -> printAttach $ filter typeDefs $ filter (byName "vir") $ grabFunctions x
                        Left (ParseError (strings, _)) -> print $ head strings
                      return ()

printAttach = prettyList . map stringed
    where stringed (return, name, args) = "attach_function " ++ ":" ++ head name ++ ", " ++ handleArgs ++ ", " ++ (head $ argsToString [(handleReturn (head return) args)])
              where handleArgs | args == [] = "[]"
                               | (head $ reverse args) /= "ptr" = "[" ++ (concat $ intersperse ", " $ argsToString args) ++ "]"
                               | otherwise = "[" ++ (concat $ intersperse ", " $ argsToString $ reverse $ tail $ reverse args) ++ "]"

prettyList [] = return ()
prettyList (x:xs) = do putStrLn x
                       prettyList xs


argsToString = reverse . (toString [])
    where toString acc [] = acc
          toString acc (x:xs) | "* *" `isInfixOf` x = toString (":pointer":acc) xs
                              | "unsigned char *" `isInfixOf` x = toString (":pointer":acc) xs
                              | "char *" `isInfixOf` x  = toString (":string":acc) xs
                              | "void *" `isInfixOf` x = toString (":void_pointer":acc) xs
                              | "int" `isInfixOf` x  = toString (":int":acc) xs
                              | "unsigned long long" `isInfixOf` x = toString ( ":ulong_long":acc) xs
                              | "unsigned long" `isInfixOf` x = toString ( ":ulong":acc) xs
                              | otherwise = toString ((":" ++ (head $ words x)):acc) xs

handleReturn return [] = return
handleReturn return args | "ptr" == (head $ reverse args) = return ++ " *"
                         | otherwise = return

namesOnly (_, [], _) = ""
namesOnly (_, foo, _) = head foo

byName _ (_, [], _) = False
byName name (_, names, _) =  name `isInfixOf` (show $ head names)

typeDefs (defs, _, _) = not ("typedef" `elem` defs)

grabFunctions = funcDeclr

funcDeclr [] = []
funcDeclr (x:xs) = (funcDeclr' x) : (funcDeclr xs)
    where funcDeclr' (CDeclExt (CDecl declSpecs declrs _)) = (typeSpec declSpecs, grabDeclrsPart ident declrs, concat $ grabDeclrsPart derivedFunction declrs)

grabDeclrsPart f =  (map f) . catMaybes . map (\(d, _, _) -> d)

typeSpec = map (show . pretty)

ident (CDeclr (Just (Ident id _ _)) _ _ _ _) = id

derivedFunction (CDeclr _ declarations _ _ _) = funcDeclrIdents
    where funcDeclrIdents = concat $ map extract declarations
              where extract (CFunDeclr (Left idents) _ _) = map identToString idents
                    extract (CFunDeclr (Right (c, b)) _ _) = (map (show . pretty) c)
                    extract (CPtrDeclr typeQ node ) = ["ptr"]
                    extract (CArrDeclr _ _ _ ) = ["arr"]

typeSpecs [] = []
typeSpecs (x:xs) = (typeSpecs' x) : (typeSpecs xs)
   where typeSpecs' (CDeclExt (CDecl specs _ _)) = (\( _ , _ , _ , specs , _ ) -> specs) $ partitionDeclSpecs specs

