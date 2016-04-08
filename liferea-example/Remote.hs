{-# LANGUAGE TemplateHaskell #-}

module Remote where

import DBus

makeDbusEndpoints def "liferea-example/liferea.xml"
