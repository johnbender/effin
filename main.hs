
module Main where
import Language.C
import Language.C.Syntax.AST
import Language.C.Data.Ident
import Language.C.Analysis
import Language.C.Analysis.TravMonad
import Language.C.System.GCC   -- preprocessor used
import Data.List
import Data.Maybe
import Data.Either

main = print 'x'

getAnalysed = do Right (CTranslUnit x _) <- parseCFile (newGCC "gcc") Nothing [] "VBoxCAPI_v3_2.h"
                 mapM (print . pretty) $ (grabFunIdents . grabProperties . grabStructMembers . grabStructs) x

grabFunIdents = concat . (map fst) . rights . (map funDeclr)

grabProperties = concat . (map (snd . (fromMaybe ("", [])) . properties))

grabStructMembers = concat . (map cdeclToCdeclr) . concat . (map ((fromMaybe []) . structMemberDeclarations))

grabStructs = structs . concat . typeSpecs

typeSpecs [] = []
typeSpecs (x:xs) = (typeSpecs' x) : (typeSpecs xs)
   where typeSpecs' (CDeclExt (CDecl specs _ _)) = (\( _ , _ , _ , specs , _ ) -specs) $ partitionDeclSpecs specs

structs [] = []
structs ((CSUType x _):xs) = x : structs xs
structs (x:xs) = structs xs

structName (CStruct _ (Just (Ident name  _ _ )) _ _ _) = name
structName _ = ""

structMemberDeclarations (CStruct _ _ decls _ _) = decls

properties (Just (CDeclr (Just (Ident name _ _ ) ) derived _ _ _ )) | "Get" `isInfixOf` name = Just (name, derived)
                                                                    | "Set" `isInfixOf` name = Just (name, derived)
                                                                    | otherwise = Nothing
properties _ = Nothing

cdeclToCdeclr (CDecl _ declList _ ) = map (\(x,_,_) -x) declList

funDeclr (CFunDeclr newOrOld _ _ ) = newOrOld
funDeclr _ = Left []