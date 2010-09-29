module Main where
import Language.C as C
import Language.C.Syntax.AST
import Language.C.Data.Ident
import Language.C.Analysis
import Language.C.Analysis.TravMonad
import Language.C.System.GCC   -- preprocessor used
import Data.List
import Data.Maybe
import Data.Either
import Control.Monad
import Control.Monad.Writer

data Object = Object Main.Name [CDecl]

data Member = Function Main.Name [Member]
            | Property Main.Name Main.TypeName

type TypeName = String
type Name = String

class Named a where
    name :: a -> Main.Name

instance Named Object where
    name (Object x _ ) = x

instance Named Member where
    name (Function x _) = x
    name (Property x _) = x

instance Show Object where
    show (Object name decls) | name /= "" = "Object (" ++ name ++ " [" ++ ( concat $ map (show . pretty) decls) ++ "]" ++ ")"
                             | otherwise = ""

main = print 'x'

getAnalysed = do Right (CTranslUnit x _) <- parseCFile (newGCC "gcc") Nothing [] "foo.h"
                 mapM (print) $ grabFunctions x
--                 mapM print $ catMaybes $ record x
    -- where record = (mapM print) . (grabPropertyNames . grabStructMembers . grabStructs)


record :: [CExtDecl] -> [Maybe Object]
record = grabStructs

grabFunctions = (map ident) . concat . funcDeclr
    where ident (CDeclr (Just (Ident id _ _)) _ _ _ _) = id

grabStructs = (map nameAndDecls) . structs . concat . typeSpecs
    where nameAndDecls (CStruct _ (Just (Ident id _ _)) (Just decls) _  _ ) = Just (Object id decls)
          nameAndDecls _ = Nothing

funcDeclr [] = []
funcDeclr (x:xs) = (funcDeclr' x) : (funcDeclr xs)
    where funcDeclr' (CDeclExt (CDecl _ declrs _)) = catMaybes $ map (\(d, _, _) -> d) declrs

typeSpecs [] = []
typeSpecs (x:xs) = (typeSpecs' x) : (typeSpecs xs)
   where typeSpecs' (CDeclExt (CDecl specs _ _)) = (\( _ , _ , _ , specs , _ ) -> specs) $ partitionDeclSpecs specs

functions [] = []
functions ((CSUType y _):xs) = y : structs xs
functions (x:xs) = structs xs

structs [] = []
structs ((CSUType y _):xs) = y : structs xs
structs (x:xs) = structs xs

structName (CStruct _ (Just (Ident name  _ _ )) _ _ _) = name
structName _ = ""

structMemberDeclarations (CStruct _ _ decls _ _) = decls


grabFunIdents = concat . (map fst) . rights . (map funDeclr)

grabPropertyDeclarations = propertyFilter snd

--grabPropertyNames = tell . (filter (/= "")) . (propertyFilter fst)

propertyFilter f = (map (f . (fromMaybe ("", [])) . properties))

grabStructMembers = concat . (map cdeclToCdeclr) . concat . (map ((fromMaybe []) . structMemberDeclarations))
    where cdeclToCdeclr (CDecl _ declList _ ) = map (\(x,_,_) -> x) declList


properties (Just (CDeclr (Just (Ident name _ _ ) ) derived _ _ _ )) | length derived == 2 = Just (name, derived)
                                                                    | otherwise = Nothing
properties _ = Nothing

funDeclr (CFunDeclr newOrOld _ _ ) = newOrOld
funDeclr _ = Left []