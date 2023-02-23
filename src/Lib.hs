module Lib
  ( main,
  )
where

import qualified Blog.Menu as M
import Data.Foldable
import Data.Function
import Data.Maybe
import Data.Traversable
import qualified Hakyll as H
import Polysemy
import System.FilePath.Posix (normalise)
import Text.Blaze.Html.Renderer.Pretty

data Site m a where
  Site :: Site m ()

makeSem ''Site

toSite :: (Member (Embed IO) r) => Sem (Site ': r) a -> Sem r a
toSite =
  interpret
    ( \case
        Site ->
          embed $
            H.hakyll $
              sequence_
                [ compileTemplates,
                  compileContent
                ]
    )

main :: IO ()
main = toSite site & runM

content :: H.Pattern
content = "**.md"

compileContent :: H.Rules ()
compileContent =
  sequence_
    [ compileMenu,
      compileMarkdown
    ]

compileTemplates :: H.Rules ()
compileTemplates = H.match "templates/*" $ H.compile H.templateBodyCompiler

compileMenu :: H.Rules ()
compileMenu = H.match content $ do
  H.version "menu" $ H.compile $ do
    item <- H.setVersion Nothing <$> H.getUnderlying
    route <- H.getRoute item
    asum (H.makeItem <$> route)

compileMarkdown :: H.Rules ()
compileMarkdown = H.match content $ do
  H.route $ H.setExtension "html"
  H.compile $ do
    ctx <- contentContext
    H.getResourceBody
      >>= H.loadAndApplyTemplate "templates/default.html" ctx
      >>= H.relativizeUrls

contentContext :: H.Compiler (H.Context String)
contentContext = do
  menu <- getMenu
  return $
    H.constField "menu" menu
      <> H.defaultContext

getMenu :: H.Compiler String
getMenu = do
  route <- H.getRoute =<< H.getUnderlying
  case route of
    Nothing -> H.noResult "No current route"
    Just r -> do
      items <- H.loadAll $ H.fromVersion $ Just "menu" :: (H.Compiler [H.Item String])
      let menu = M.makeMenu (fmap H.itemBody items)
      return (fromMaybe "" (renderHtml . M.showMenu <$> (M.moveTo r =<< menu)))
