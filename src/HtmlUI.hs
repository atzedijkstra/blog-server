{-# LANGUAGE OverloadedStrings, TemplateHaskell, ParallelListComp #-}

------------------------------------------------------------------------------
-- | This module is where the Html (form) interfaces are
module HtmlUI
  -- all is exported
  where

------------------------------------------------------------------------------
import           Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
-- import qualified Data.Graph.Inductive as G
import           Control.Lens
import           Data.Monoid
import qualified Data.Text as T
import           Control.Monad
import           Control.Monad.State
import           Control.Applicative
import qualified Prelude as P
import           Prelude
import           Snap.Snaplet.Auth (AuthUser(..))
------------------------------------------------------------------------------
import           Text.Blaze.Html
import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Renderer.XmlHtml
import           Text.Digestive as D
import           Text.Digestive.Blaze.Html5
-- import           Text.Digestive.Heist
-- import           Text.Templating.Heist
import qualified Text.XmlHtml as X
------------------------------------------------------------------------------
import           Application.User
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Utils
------------------------------------------------------------------------------

{-
------------------------------------------------------------------------------
-- | List of forms to form resulting in list
formFlatten :: (Monad m, Monoid v) => [Form v m a] -> Form v m [a]
formFlatten = foldr (\f fs -> (:) <$> f <*> fs) (pure [])

------------------------------------------------------------------------------
-- | Instances
instance ToMarkup Data where
  toMarkup d = toMarkup (show d)

------------------------------------------------------------------------------
-- | Map over DTable
mapDTable :: (x->y) -> DTable' x -> DTable' y
mapDTable f = P.map (P.map f)

------------------------------------------------------------------------------
-- | Make Html table
dtableToHtml :: ToMarkup x => Maybe (DCol' x) -> Maybe (DRow' x) -> DTable' x -> H.Html
dtableToHtml mbRowHdr mbColHdr dtbl = H.table ! customAttribute "border" "1" $ do
    mapM_ (H.tr . sequence_)
    $ zipWith (++) rowhdr
    $ (colhdr ++)
    $ mapDTable (H.td . toMarkup) dtbl
  where colhdr = maybe [] (\c -> [P.map (H.th . toMarkup) c]) mbColHdr
        rowhdr = maybe (repeat []) 
                   (\r -> P.map ((:[]) . H.th) 
                          $ maybe [] (const [mempty]) mbColHdr
                            ++ P.map toMarkup r
                            ++ repeat mempty
                   ) mbRowHdr
-}

------------------------------------------------------------------------------
-- Forms
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | CreateCourse form
{-
createCourseForm :: Monad m => Form T.Text m CreateCourse
createCourseForm = CreateCourse
  <$> "course" .: check "Must be non-empty" (not . T.null) (text Nothing)
  <*> "nrRows" .: intFld
  <*> "nrCols" .: intFld
  where intFld = check "Must be >0" (> 0) $ stringRead "Must be integer" (Just 1)
-}

------------------------------------------------------------------------------
-- | User form
editFormUser :: Monad m => User -> Form T.Text m User
editFormUser u = (\n e -> (userName .~ n) . (authUser %~ \au -> au {userEmail = Just e}) $ u)
    <$> "name"  .: D.text (Just $ u ^. userName)
    <*> "email" .: D.text (userEmail $ u ^. authUser)

------------------------------------------------------------------------------
-- | EditCourseData form
{-
editCourseDataForm
  :: Monad m
  => AcctId
  -> MarksAdm
  -> EditCourseData
  -> ( Form T.Text m EditCourseData							-- ^ the form which is going to be filled in
     , Splice m												-- ^ the rendering of the form
     , DTable' T.Text -> UpdatedDataMp T.Text				-- ^ the computation of a map of update locations and their value, given the table from the form
     )
editCourseDataForm acctId ma prev
  = (f, return $ renderHtmlNodes h, mbMkUpdMp)
  where (f, h, mbMkUpdMp) = case prev of
            EditCourseData {_edcoursedRoleAccess=Nothing}
              -> ( (\rap -> edcoursedRoleAccess ^= Just rap $ prev) 
                   <$> ran .: choice choices Nothing
                 , dtableToHtml Nothing Nothing [ [toHtml ran, dfInputSelect ran] ]
                 , const Map.empty
                 )
              where choices =
                      [ ((r,a,p), showRA rn an p)
                      | (a,rps) <- Map.toList $ acctAccessIdLookup acctId ma
                      , (r,p) <- rps
                      , (rn,an) <- maybeToList $ lkupRaM r a
                      ]
            EditCourseData {_edcoursedRoleAccess= Just rap@(r,a,p), _edcoursedName=Nothing}
              -> ( (\c reset -> edcoursedName ^= Just c $ edcoursedReset ^= reset $ prev)
                   <$> cn .: choice choices Nothing
                   <*> resetn .: bool Nothing
                 , do dtableToHtml Nothing Nothing [ htmlRAP r a p, [toHtml cn, dfInputSelect cn] ]
                      htmlReset
                 , const Map.empty
                 )
              where choices = [ (nmt, nmt) | (_,nm) <- coursesLookup acctId a (Just r) ma, let nmt = T.pack nm ]
            EditCourseData {_edcoursedRoleAccess= Just rap@(r,a,p), _edcoursedName=Just courseNm}
              -> ( (\reset tbl -> edcoursedTable ^= Just tbl $ edcoursedReset ^= reset $ prev)
                   <$> resetn .: bool Nothing
                   <*> formFlatten [ formFlatten [ tfldnm c .: text (Just $ T.pack $ show $ c ^. lcdrData) | c <- r ] | r <- cdata ]
                 , do dtableToHtml Nothing Nothing [ htmlRAP r a p, htmlC courseNm ]
                      htmlReset
                      H.hr
                      toHtml cd
                      dtableToHtml (Just $ P.map toHtml rns) (Just $ P.map toHtml cns)
                        $ mapDTable tfld cdata
                 , \newtbl -> flip execState Map.empty $ sequence_
                      [ unless (show (old ^. lcdrData) == T.unpack new)
                               (modify $ Map.insert (old ^. lcdrDataId) new)
                      | ro <- cdata, old <- ro | rn <- newtbl, new <- rn
                      ]
                 )
              where cdata = courseDataLookup acctId a (Just r) (T.unpack courseNm) ma
                    rns = [ P.head r ^. lcdrRowNm | r <-        cdata ]
                    cns = [ c        ^. lcdrColNm | c <- P.head cdata ]
                    tfldnm' d = dpathRowCol (d ^. lcdrRowNm) (d ^. lcdrColNm)
                    tfldnm = T.pack . tfldnm'
                    tfld d | d ^. lcdrMayUpdate = dfInput (tfldnm d) ! size "5"
                           | otherwise          = toHtml $ d ^. lcdrData

        -- names of form fields
        cn  = "Course"
        cd  = ("Course Data" :: T.Text)
        ran = "Role and Access"
        resetn = "Reset"
        
        -- dealing with coursename
        htmlC c = [toHtml cn, toHtml c]

        -- dealing with role & access
        showRA rn an p = T.pack $ "Role " ++ show rn ++ " for " ++ show an ++ " " ++ show (Set.toList p)
        htmlRAP r a p = [toHtml ran, toHtml $ showRA rn an p]
          where (rn,an) = fromJust $ lkupRaM r a
        lkupRaM r a = do 
          rn <- nmidrelLookupId2Nm roleNm r $ ma ^. roles
          an <- nmidrelLookupId2Nm accNm a $ ma ^. accesses
          return (rn,an)
        
        -- reset bool (for lack of better solution)
        htmlReset = do {toHtml resetn; dfInputCheckbox resetn}
-}

------------------------------------------------------------------------------
-- | CreateCourse view
{-
createCourseView :: View H.Html -> H.Html
createCourseView view = do
  label     "course" view "Course: "
  inputText "course" view
  H.br

-}

{-
testSplice :: Monad m => Splice m
testSplice = do
  -- return [X.TextNode "test"]
  return $ renderHtmlNodes $ em "test"

testViewSplice :: Monad m => Splice m
testViewSplice = do
    -- return [X.TextNode "test"]
    return $ renderHtmlNodes $ do
      em "test"
      dfInput "course"
  -- where dfInput r = HI.Parent "dfInput" "<dfInput" "</dfInput>" ! HI.attribute "ref" " ref=\"" r $ HI.Empty
-}
