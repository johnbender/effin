
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writerb

recordEntries :: FilePath -> WriterT [String] IO ()
recordEntries path = do
  file <- liftIO . readFile $ path
  forM_ (lines file) $ \line ->
      tell [line]