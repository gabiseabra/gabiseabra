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

type Count
  = { totalCount :: Int }

type Language
  = { name :: String, color :: Maybe String }

type Repo
  = { name :: String
    , description :: Maybe String
    , isFork :: Boolean
    , primaryLanguage :: Language
    }

type ReposQuery
  = { viewer ::
        { repos :: Connection Repo
        , forks :: Count
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
        forks: repositories(isFork: true) {
          totalCount
        }
        repos: repositories(first: 100, isFork: false) {
          nodes {
            name
            description
            isFork
            primaryLanguage {
              name
              color
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
