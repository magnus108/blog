module Lib
    ( main
    )
where

import qualified Blog.Menu as M

import qualified Hakyll as H


content :: H.Pattern
content = "**/index.md"


main :: IO ()
main = H.hakyll $ sequence_ [ compileContent
                            ]


compileContent :: H.Rules ()
compileContent = sequence_ [ compileMenu
                           , compileMarkdown
                           ]


compileMenu :: H.Rules ()
compileMenu = H.match content $ do
    H.version "menu" $ H.compile $ do
        item  <- H.setVersion Nothing <$> H.getUnderlying
        route <- H.getRoute item
        case route of
            Nothing -> H.noResult "No menu item"
            Just r  -> H.makeItem r


compileMarkdown :: H.Rules ()
compileMarkdown = H.match content $ do
    H.route $ H.setExtension "html"
    H.compile $ do
        H.getResourceBody


contentContext :: H.Compiler (H.Context String)
contentContext = do
    menu <- getMenu
    return
        $  H.constField "menu" menu
        <> H.defaultContext


getMenu :: H.Compiler String
getMenu = do
    route <- H.getRoute =<< H.getUnderlying
    case route of
        Nothing -> H.noResult "No current route"
        Just r  -> do
            items <- H.loadAll $ H.fromVersion $ Just "menu" :: (H.Compiler [H.Item String])
            --let menu = M.fromTreeZipper $ TZ.fromList r $ fmap itemBody items
            return "bob" -- $ renderHtml menu
