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

getAnalysed = handleParse <=< parseCFile (newGCC "gcc") Nothing []

handleParse (Right (CTranslUnit x _)) =
    printAttachDefinitions $ filterUnneeded $ selectFunctionDeclarations x
handleParse (Left (ParseError (strings, _))) = print $ head strings

filterUnneeded = filter typeDefs . filter (byName "vir")

selectFunctionDeclarations [] = []
selectFunctionDeclarations (x:xs) = (funcDeclr' x) : (selectFunctionDeclarations xs)
    where funcDeclr' (CDeclExt (CDecl declSpecs declrs _)) =
              let typeSpecification = typeSpec declSpecs
                  identifier = grabDeclrsPart ident declrs
                  functionArg = concat $ grabDeclrsPart derivedFunction declrs
              in (typeSpecification, identifier, functionArg)

printAttachDefinitions = foldr (>>) (return ()) . map (putStrLn . functionToString)

functionToString (return, name, args) = let method = "attach_function "
                                            functionName = ":" ++ head name
                                            functionArgs = handleArgs args
                                            returnType = (head $ mapArgs [(handleReturn (head return) args)])
                                        in method ++ (intercalate ", " [functionName, functionArgs, returnType])

handleArgs [] = "[]"
handleArgs args = "[" ++ (intercalate ", " $ mapArgs $ dropWhile (== "ptr") args) ++ "]"

mapArgs = reverse . (toString [])
    where toString acc [] = acc
          toString acc (x:xs) | "* *" `isInfixOf` x = toString (":pointer":acc) xs
                              | "unsigned char *" `isInfixOf` x = toString (":pointer":acc) xs
                              | "char *" `isInfixOf` x  = toString (":string":acc) xs
                              | "void *" `isInfixOf` x = toString (":void_pointer":acc) xs
                              | "unsigned long long" `isInfixOf` x = toString ( ":ulong_long":acc) xs
                              | "unsigned long" `isInfixOf` x = toString ( ":ulong":acc) xs
                              | otherwise = toString ((":" ++ (head $ words x)):acc) xs

handleReturn return [] = return
handleReturn return args | "ptr" == last args = return ++ " *"
                         | otherwise = return

namesOnly (_, [], _) = ""
namesOnly (_, foo, _) = head foo

byName _ (_, [], _) = False
byName name (_, names, _) =  name `isInfixOf` (show $ head names)

typeDefs (defs, _, _) = not ("typedef" `elem` defs)

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

