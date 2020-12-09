module Hey.Api.Github where

import Prelude
import Affjax as AX
import Affjax.RequestBody as Req
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as Res
import Data.Argonaut (encodeJson)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Hey.Hooks.UseFetch (Fetch(..))

type Connection a
  = { pageInfo ::
        { hasNextPage :: Boolean
        , endCursor :: String
        }
    , nodes :: Array a
    }

type Connection' a
  = { nodes :: Array a }

type Language
  = { name :: String, color :: Maybe String }

type Repo
  = { name :: String
    , description :: Maybe String
    , isFork :: Boolean
    , languages :: Connection' Language
    }

type ReposQuery
  = { viewer ::
        { repositories ::
            Connection Repo
        }
    }

fetchRepos :: Fetch { data :: ReposQuery }
fetchRepos = Fetch "github/repos" req
  where
  req =
    AX.defaultRequest
      { url = "https://api.github.com/graphql"
      , headers = [ RequestHeader "Authorization" "bearer e2cdad17d8b6308897f05edaace521b1964cfb0e" ]
      , content = Just $ Req.json $ encodeJson { query }
      , method = Left POST
      , responseFormat = Res.json
      }

  query =
    """
    query {
      viewer {
        repositories(first: 100) {
          nodes {
            name
            description
            isFork
            languages(first: 3) {
              nodes {
                name
                color
              }
            }
          }
          pageInfo {
            hasNextPage
            endCursor
          }
        }
      }
    }
    """
