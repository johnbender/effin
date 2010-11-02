module Main where
import Language.C
import Language.C.Data.Ident
import Language.C.System.GCC
import Data.List
import Data.Maybe
import Control.Monad
import System( getArgs )
import System.Console.GetOpt

main = do
  args <- getArgs
  let (flags, nonOpts, msgs) = getOpt RequireOrder options args
  case nonOpts of
    [] -> putStrLn "Usage"
    otherwise -> printFFI $ head nonOpts
  return ()

data Flag = Version

options = [ Option ['V'] ["version"] (NoArg Version) "show version number" ]

printFFI = handleParse <=< parseCFile (newGCC "gcc") Nothing []

handleParse (Right (CTranslUnit x _)) =
    printAttachDefinitions $ filterUnneeded $ map (declarationToTriple) x
handleParse (Left (ParseError (strings, _))) = print $ head strings

filterUnneeded = filter typeDefs . filter (byName "vir")

declarationToTriple (CDeclExt (CDecl declSpecs declrs _)) =
    let typeSpecification = typeSpec declSpecs
        identifier = grabDeclrsPart ident declrs
        functionArg = concat $ grabDeclrsPart derivedFunction declrs
    in (typeSpecification, identifier, functionArg)

printAttachDefinitions = foldr (>>) (return ()) . map (putStrLn . functionToString)

functionToString (return, name, args) =
    let method = "attach_function "
        functionName = ":" ++ head name
        functionArgs = handleArgs args
        returnType = (head $ mapArgs [(handleReturn (head return) args)])
    in method ++ (intercalate ", " [functionName, functionArgs, returnType])

handleArgs [] = "[]"
handleArgs args = "[" ++ (intercalate ", " $ mapArgs $ dropWhile (== "ptr") args) ++ "]"


-- TODO move to fold
mapArgs = reverse . (toString [])
    where toString acc [] = acc
          toString acc (x:xs) = toString ((typeToSymbol x):acc) xs

typeToSymbol string = let matches = find (\(k, a) -> k `isInfixOf` string) typeMap
                      in case matches of
                        Nothing -> ":" ++ (head $ words string)
                        Just (k, a) -> a

-- TODO regexes
typeMap = [("char *", ":string")
          ,("void *", ":void_pointer")
          ,("unsigned long long", ":ulong_long")
          ,("unsigned long", ":ulong")
          ,("*", ":pointer")]

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

-- TODO move to map
typeSpecs [] = []
typeSpecs (x:xs) = (typeSpecs' x) : (typeSpecs xs)
   where typeSpecs' (CDeclExt (CDecl specs _ _)) = (\( _ , _ , _ , specs , _ ) -> specs) $ partitionDeclSpecs specs
