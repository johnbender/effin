
> module Main where
> import Language.C
> import Language.C.Analysis
> import Language.C.Analysis.TravMonad
> import Language.C.System.GCC   -- preprocessor used

> main = do Right x <- parseCFile (newGCC "gcc") Nothing [] "VBoxCAPI_v3_2.h"
>           (print . pretty) $ filterGlobalDecls structs $ traverse x
>     where structs (TagEvent (CompDef (CompType ref _ _ _ _ ))) = True
>           structs _ = False
>           traverse y = case runTrav $ analyseAST y of
>                        Right (a, _) -> a
>                        _ -> undefined

> structs trav = withExtDeclHandler trav printStruct
>     where printStruct (TagEvent (CompDef (CompType ref _ _ _ _ ))) = modifyUserState (\s -> ref:s)
