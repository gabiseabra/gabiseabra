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
import Hey.Data.JSON (JDateTime)
import Hey.Hooks.UseFetch (Fetch(..))

type Connection a
  = { nodes :: Array a }

type Count
  = { totalCount :: Int }

type Language
  = { name :: String, color :: Maybe String }

type RepoInfo
  = { name :: String
    , primaryLanguage :: Language
    }

type Repo
  = { name :: String
    , description :: Maybe String
    , url :: String
    , homepageUrl :: Maybe String
    , createdAt :: JDateTime
    , languages :: Connection Language
    }

type User
  = { repositories :: Connection RepoInfo
    , contributions :: Connection RepoInfo
    , featured :: Connection Repo
    , forks :: Count
    }

type ViewerQuery
  = { viewer :: User
    }

fetchViewer :: String -> Fetch { data :: ViewerQuery }
fetchViewer token = Fetch "github/viewer" req
  where
  req =
    AX.defaultRequest
      { url = "https://api.github.com/graphql"
      , headers = [ RequestHeader "Authorization" $ "bearer " <> token ]
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
        repositories: repositories(first: 100, isFork: false) {
          nodes { ...RepoInfo }
        }
        contributions: repositoriesContributedTo(first: 100) {
          nodes { ...RepoInfo }
        }
        featured: pinnedItems(first: 6, types: [REPOSITORY]) {
          nodes {
            ... on Repository { ...Repo }
          }
        }
      }
    }

    fragment Language on Language {
      name
      color
    }

    fragment RepoInfo on Repository {
      name
      primaryLanguage { ...Language }
    }

    fragment Repo on Repository {
      name
      description
      url
      homepageUrl
      createdAt
      languages(first: 3) {
        nodes { ...Language }
      }
    }
    """
