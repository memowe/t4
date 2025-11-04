import T4.Data
import T4.Storage
import qualified Data.Set as S
import Control.Monad.IO.Class
import Control.Lens
import Data.Aeson
import Servant
import Data.OpenApi hiding (Tag, Server, server)
import Servant.OpenApi
import Servant.Swagger.UI
import Network.Wai.Handler.Warp
import qualified Paths_terminal_time_tracking_tool as Paths

instance ToSchema Clock where
  declareNamedSchema _ = do
    return $ NamedSchema (Just "Clock") $ mempty
      & type_   ?~ OpenApiObject
      & example ?~ toJSON clock
    where clock = In  (SLT (read "2025-11-04 09:15:00"))
                      (Just "T4 programming")
                      (S.fromList ["backend", "api", "haskell"])

t4OpenApi :: OpenApi
t4OpenApi = toOpenApi (Proxy :: Proxy T4API)
  & info . title        .~ "T4: Terminal Time Tracking Tool API"
  & info . description  ?~ "Read-only API for T4 time tracking"

type API  =   T4API
        :<|>  "openapi.json"  :> Get '[JSON] OpenApi
        :<|>  SwaggerSchemaUI "swagger-ui" "swagger.json"
        :<|>  Raw

type T4API  =                     Get '[JSON] (Maybe Clock)
          :<|>  "clocks"      :>  Get '[JSON] [Clock]
          :<|>  "categories"  :>  Get '[JSON] [Category]
          :<|>  "tags"        :>  Get '[JSON] [Tag]

server :: FilePath -> Server API
server staticDir  =   apiServer
                :<|>  pure t4OpenApi
                :<|>  swaggerSchemaUIServer t4OpenApi
                :<|>  serveDirectoryFileServer staticDir

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
  putStrLn "T4 API Server starting..."
  putStrLn "  OpenAPI spec at:  http://localhost:8080/openapi.json"
  putStrLn "  Swagger UI at:    http://localhost:8080/swagger-ui"
  putStrLn "  API endpoints:    http://localhost:8080/*"
  run 8080 $ serve (Proxy :: Proxy API) $ server staticDir
