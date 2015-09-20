{-# LANGUAGE
    ExtendedDefaultRules
  , OverloadedStrings
  #-}

module STM.Templates where

import Lucid
import Lucid.Base


masterTemplate :: Monad m => HtmlT m () -> HtmlT m ()
masterTemplate content =
  doctypehtml_ $ do
    head_ $ do
      title_ "STM Nonce Cache Test"
      meta_ [charset_ "utf8"]
    body_ [] content

loginPage :: Monad m => HtmlT m ()
loginPage = masterTemplate $
  form_ [action_ "/login", method_ "POST"] $ do
    input_ [name_ "user", type_ "text", placeholder_ "Username"]
    input_ [name_ "password", type_ "password"]
    input_ [type_ "submit", value_ "Submit"]
