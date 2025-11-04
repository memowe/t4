import T4.Data
import T4.Storage
import qualified Data.Set as S
import Control.Monad.IO.Class
import Servant
import Network.Wai.Handler.Warp
import qualified Paths_terminal_time_tracking_tool as Paths

type API = "api" :> T4API :<|> Raw

type T4API  =                    Get '[JSON] (Maybe Clock) -- status
          :<|>  "clocks"      :> Get '[JSON] [Clock]
          :<|>  "categories"  :> Get '[JSON] [Category]
          :<|>  "tags"        :> Get '[JSON] [Tag]

server :: FilePath -> Server API
server staticDir = apiServer :<|> serveDirectoryFileServer staticDir

apiServer :: Server T4API
apiServer = getStatus
      :<|>  getClocks
      :<|>  getCategories
      :<|>  getTags

  where getStatus     = findMax <$> clocks
        getClocks     = S.toList <$> clocks
        getCategories = S.toList . allCategories <$> clocks
        getTags       = S.toList . allTags <$> clocks
        clocks        = do  dir <- liftIO getStorageDirectory
                            liftIO $ loadDataFromDir dir
        findMax       = fmap fst . S.maxView

main :: IO ()
main = do
  staticDir <- Paths.getDataFileName "exe-t4-api/static"
  putStrLn "Listening on http://localhost:8080..."
  run 8080 $ serve (Proxy :: Proxy API) $ server staticDir
