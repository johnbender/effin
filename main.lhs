
> module Main where
> import Language.C
> import Language.C.System.GCC   -- preprocessor used
> main = do Right x <- parseCFile (newGCC "gcc") Nothing [] "VBoxCAPI_v3_2.h"
>           (print . pretty) x

