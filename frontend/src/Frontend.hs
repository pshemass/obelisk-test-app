{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
module Frontend where

import qualified Data.Text as T
import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom.Core
import Reflex.Dom.Widget.ECharts
import Reflex.Dom.ACE

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Fix (MonadFix)
import Control.Monad
import Control.Lens

import qualified Data.Map as Map
import Common.Api
import Common.Route
import Obelisk.Generated.Static

import Data.Text (Text)
import Language.Javascript.JSaddle hiding ((!!))

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
    el "title" $ text "Obelisk Minimal Example"
    elAttr "style" ("type" =: "text/css" <> "media" =: "screen") $
      text $ T.unlines
        [ "#ace-editor { width:100%; height:100%; }"
        , "#editor { position:relative; height:400px; width:600px; left:-10px; padding:10px; }"
        , "body { width:100%; height:100%; }"
        , "input { width:600px; }"
        ]
    elAttr "script" ("type" =: "text/javascript" <> "src" =: static @"echarts.min.js") blank
  , _frontend_body = do
      text "Welcome to Obelisk!"
      el "p" $ text $ T.pack commonStuff
      -- elAttr "div" ("style" =: "padding: 50px;") delayedRender [basicLineChart]
      app $ Just "xxx"
            -- ace <- divClass "yourACEWrapperDiv" $ -- wrapper div not required
      --  void $ widgetHold blank $ ffor evScriptLoaded $ const $ do
      --    aceWidget def (AceDynConfig Nothing) never "initial editor contents"
      -- elAttr "img" ("src" =: static @"obelisk.jpg") blank
      return ()
  }

app
  :: forall t m js .
     ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     , PerformEvent t m
     , TriggerEvent t m
     , Prerender js t m
     )
  => Maybe Text
  -> m ()
app _ = void $ prerender_ blank $ elAttr "div" ("style" =: "display: flex; flex-wrap: wrap") $ do
   delayedRender[ basicLineChart]
   (script, _ ) <- elAttr' "script" ("src" =: static @"ace.js") blank
   let evScriptLoaded :: Event t ()
       evScriptLoaded = () <$ domEvent Load script
       -- loading = el "pre" $ text "loading..."
   void $ widgetHold blank $ ffor evScriptLoaded $ const $ do
     void $ elAttr "div" ("id" =: "editor") $ do
      let cfg = def{ _aceConfigBasePath        = Just "/"
                   , _aceConfigElemAttrs       = "id" =: "ace-editor"
                   , _aceConfigWordWrap        = True
                   , _aceConfigShowPrintMargin = True
                   }
      aceWidget cfg (AceDynConfig Nothing) never "blah"
   return ()



delayedRender [] = return ()
delayedRender (m:ms) = do
        c <- wrapper m
        (ev1, _) <- headTailE (_chart_finished c)
        void $ widgetHold blank $ ffor ev1 $ \_ -> do
          delayedRender ms
wrapper m = elAttr "div" ("style" =: "padding: 50px;") m

tickWithSpeedSelector
  :: ( PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadFix m
     , MonadHold t m
     , GhcjsDomSpace ~ DomBuilderSpace m
     , TriggerEvent t m
     , MonadIO (Performable m)
     )
  => m (Event t TickInfo)
tickWithSpeedSelector = el "div" $ do
  el "label" $ text "Tick Interval"
  r <- rangeInput $ def
    & rangeInputConfig_initialValue .~ 1
    & rangeInputConfig_attributes .~ constDyn (("min" =: "0.1") <> ("max" =: "2") <> ("step" =: "0.1"))
  dynText $ (\v -> tshow v <> "s") <$> (value r)
  dyn ((\v -> tickLossyFromPostBuildTime (fromRational $ toRational v)) <$> (value r))
    >>= switchHold never


basicLineChart
  :: ( PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadHold t m
     , GhcjsDomSpace ~ DomBuilderSpace m
     , TriggerEvent t m
     , MonadFix m
     , MonadIO (Performable m)
     , MonadJSM m
     , MonadJSM (Performable m)
     )
  => m (Chart t)
basicLineChart = do
  tick <- tickWithSpeedSelector
  let
    f _ m = Map.fromList $ zip xAxisData $ ls ++ [l]
      where (l:ls) = map (\x -> Map.findWithDefault (DataInt 0) x m) xAxisData
  text "Enable Dynamic"
  dd <- do
    cb <- el "div" $ do
      el "label" $ text "Line 1"
      checkbox False def
    let ev = gate (current $ value cb) tick
    foldDyn f yAxisData ev
  dd2 <- do
    cb <- el "div" $ do
      el "label" $ text "Line 2"
      checkbox False def
    let ev = gate (current $ value cb) tick
    foldDyn f yAxisData2 ev

  xd <- do
    cb <- el "div" $ do
      el "label" $ text "X-axis"
      checkbox False def
    let ev = gate (current $ value cb) tick
    foldDyn (\_ (l:ls) -> ls ++ [l]) xAxisData ev

  let chartDataDyn = ((0 :: Int) =: (def, dd, xd)) <> (1 =: (dd2Series, dd2, xd))
      dd2Series = def
        & series_smooth ?~ Left True
        & series_areaStyle ?~ def

  lineChart (LineChartConfig (600, 400)
              (constDyn basicLineChartOpts)
              chartDataDyn
            )
  where
    yAxisData = Map.fromList $ zip xAxisData $ map DataInt $ reverse dataVals
    yAxisData2 = Map.fromList $ zip xAxisData $ map DataInt dataVals
    dataVals :: [Int]
    dataVals = [820, 932, 901, 934, 1290, 1330, 1320]
    xAxisData = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
    basicLineChartOpts :: ChartOptions
    basicLineChartOpts = def
      & chartOptions_yAxis .~ (def
        & axis_type ?~ AxisType_Value
        ) : []
      & chartOptions_xAxis .~ (def
        & axis_type ?~ AxisType_Category
        & axis_data ?~ (zip xAxisData $ repeat Nothing)
        ) : []

tshow :: (Show a) => a -> Text
tshow = T.pack . show

