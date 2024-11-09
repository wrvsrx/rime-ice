{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson.KeyMap qualified as A
import Data.ByteString qualified as B
import Data.ByteString.UTF8 qualified as BU
import Data.Function ((&))
import Data.List.Extra (replace)
import Data.Maybe (fromJust)
import Data.Yaml qualified as Y
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions $ do
  want [all']
  phony all' $ do
    need
      [ doubleFly
      ]
  mapM_ (\target -> phony target (copyFolderAction target)) [cnDicts, enDicts, opencc]
  phony lua $ do
    copyFolderAction lua
    writeFileChanged (buildDir </> lua </> "force_gc.lua") "local function force_gc()\ncollectgarbage(\"step\")\nend\nreturn force_gc"
  phony rimeIceDict $ do
    need [cnDicts]
    copyFileChanged "rime_ice.dict.yaml" (buildDir </> "rime_ice.dict.yaml")
  phony doubleFly $ do
    need
      [ rimeIceDict
      , lua
      , rimeIceCommon
      , symbols
      , opencc
      , meltEngSchema
      , radicalPinyinSchema
      ]
    cnt <- readFile' ("double_pinyin_" <> doubleFly <> ".schema.yaml")
    let
      cnt' = transformDoublePinyin ("_" <> doubleFly) cnt
    writeFileChanged (buildDir </> "rime_ice_double_pinyin_" <> doubleFly <> ".schema.yaml") cnt'
  phony rimeIceCommon $ do
    cnt <- readFile' "default.yaml"
    let
      bs = BU.fromString cnt
      obj = bs & Y.decodeEither' & either (error . show) id :: Y.Object
      obj' = obj & A.filterWithKey (\k _ -> k `elem` ["punctuator", "recognizer", "key_binder"])
    writeFileChanged (buildDir </> "rime_ice_common.yaml") (BU.toString (Y.encode obj'))
  phony symbols $ do
    copyFileChanged (symbols <> ".yaml") (buildDir </> symbols <> ".yaml")
  phony meltEngDict $ do
    need [enDicts]
    copyFileChanged "melt_eng.dict.yaml" (buildDir </> "melt_eng.dict.yaml")
  phony meltEngSchema $ do
    need [meltEngDict]
    cnt <- readFile' "melt_eng.schema.yaml"
    writeFileChanged (buildDir </> "melt_eng.schema.yaml") (transformDefault cnt)
  phony radicalPinyinDict $ do
    copyFileChanged "radical_pinyin.dict.yaml" (buildDir </> "radical_pinyin.dict.yaml")
  phony radicalPinyinSchema $ do
    need [radicalPinyinDict]
    cnt <- readFile' "radical_pinyin.schema.yaml"
    writeFileChanged (buildDir </> "radical_pinyin.schema.yaml") (transformDefault cnt)
 where
  buildDir = "/home/wrvsrx/.local/share/rime-data"
  all' = "all"
  cnDicts = "cn_dicts"
  rimeIceDict = "rime_ice_dict"
  enDicts = "en_dicts"
  opencc = "opencc"
  lua = "lua"
  doubleFly = "flypy"
  rimeIceCommon = "rime_ice_common"
  symbols = "symbols_caps_v"
  meltEngDict = "melt_eng_dict"
  meltEngSchema = "melt_eng_schema"
  radicalPinyinDict = "radical_pinyin_dict"
  radicalPinyinSchema = "radical_pinyin_schema"
  components =
    [ "rime-ice-common"
    , "rime-ice-double-pinyin"
    ]
  copyFolderAction dir = do
    dictList <- getDirectoryFiles "" [dir </> "*"]
    mapM_ (\x -> copyFileChanged x (buildDir </> x)) dictList
  transformDefault :: String -> String
  transformDefault =
    replace "default:" "rime_ice_common:"
      . replace "import_preset: default" "import_preset: rime_ice_common"
      . replace "lua_processor@" "lua_processor@*"
      . replace "lua_filter@" "lua_filter@*"
      . replace "lua_translator@" "lua_translator@*"
  transformDoublePinyin :: String -> String -> String
  transformDoublePinyin suffix =
    let
      src = "double_pinyin" <> suffix
      dst = "rime_ice_" <> src
     in
      replace src dst . transformDefault
