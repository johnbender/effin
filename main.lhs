
> module Main where
> import Language.C
> import Language.C.Syntax.AST
> import Language.C.Data.Ident
> import Language.C.Analysis
> import Language.C.Analysis.TravMonad
> import Language.C.System.GCC   -- preprocessor used

> main = print 'x'

> getAnalysed = do Right (CTranslUnit x _) <- parseCFile (newGCC "gcc") Nothing [] "VBoxCAPI_v3_2.h"
>                  mapM (print) ((filter (\s -> s /= "") . structs . concat . typeSpecs) x)

> typeSpecs [] = []
> typeSpecs (x:xs) = (typeSpecs' x) : (typeSpecs xs)
>    where typeSpecs' (CDeclExt (CDecl specs _ _)) = (\( _ , _ , _ , specs , _ ) -> specs) $ partitionDeclSpecs specs
>
> structs [] = []
> structs ((CSUType x _):xs) = (structName x) : structs xs
> structs (x:xs) = structs xs
>
> structName (CStruct _ (Just (Ident name@('I':xs) _ _ )) _ _ _) = name
> structName _ = ""


