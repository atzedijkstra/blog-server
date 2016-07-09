-- {-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module defines and intermediate structure to represent data in tables
module HtmlUI.DTable
  -- all is exported
  where

------------------------------------------------------------------------------
-- import           Control.Monad
-- import           Data.Monoid
-- import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
------------------------------------------------------------------------------
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- DTable
type DRow' x = [x]
type DCol' x = DRow' x
type DRow   = DRow' H.Html

-- | DTable, data view table, abstraction rows of columns
type DTable' x = [DRow' x]
type DTable = DTable' H.Html

------------------------------------------------------------------------------
-- | Map over DTable
mapDTable :: (x->y) -> DTable' x -> DTable' y
mapDTable f = map (map f)

------------------------------------------------------------------------------
-- | Make Html table
dtableToHtml :: H.ToMarkup x => Maybe (DCol' x) -> Maybe (DRow' x) -> DTable' x -> H.Html
dtableToHtml mbRowHdr mbColHdr dtbl = H.table H.! H.customAttribute "border" "1" $ do
    mapM_ (H.tr . sequence_)
    $ zipWith (++) rowhdr
    $ (colhdr ++)
    $ mapDTable (H.td . H.toMarkup) dtbl
  where colhdr = maybe [] (\c -> [map (H.th . H.toMarkup) c]) mbColHdr
        rowhdr = maybe (repeat []) 
                   (\r -> map ((:[]) . H.th) 
                          $ maybe [] (const [mempty]) mbColHdr
                            ++ map H.toMarkup r
                            ++ repeat mempty
                   ) mbRowHdr
