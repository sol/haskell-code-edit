module RenameModuleSpec (spec) where

import           Test.Hspec
import           Test.Mockery.Directory

import           RenameModule

foo :: String
foo = unlines [
    "module Foo where"
  , "fib n = .."
  ]

fooSpec :: String
fooSpec = unlines [
    "module FooSpec where"
  , "fib n = .."
  ]

bar :: String
bar = unlines [
    "module Bar where"
  , "fib n = .."
  ]

barSpec :: String
barSpec = unlines [
    "module BarSpec where"
  , "fib n = .."
  ]

spec :: Spec
spec = do
  describe "rename" $ around_ inTempDirectory $ do
    before_ ( do
      touch "src/Foo.hs"
      writeFile "src/Foo.hs" foo
      ) $ do
      it "renames a Haskell module" $ do
        rename "src/Foo.hs" "src/Bar.hs"
        readFile "src/Bar.hs" `shouldReturn` bar

      context "when module has a spec" $ do
        it "renames the spec" $ do
          touch "test/FooSpec.hs"
          writeFile "test/FooSpec.hs" fooSpec
          rename "src/Foo.hs" "src/Bar.hs"
          readFile "test/BarSpec.hs" `shouldReturn` barSpec

  describe "renameModule" $ do
    it "renames a Haskell module" $ do
      renameModule ["Foo"] ["Bar"] foo `shouldBe` bar

  describe "specFile" $ do
    it "returns spec for a given source file" $ do
      specFile "src/Foo/Bar/Baz.hs" `shouldBe` "test/Foo/Bar/BazSpec.hs"
      
  describe "specFor" $ do
    it "returns spec module for a given module" $ do
      specFor ["Foo", "Bar", "Baz"] `shouldBe` ["Foo", "Bar", "BazSpec"]

  describe "filePath" $ do
    it "returns file path from module name" $ do
      filePath "src" ["Foo", "Bar"] `shouldBe` "src/Foo/Bar.hs"

  describe "moduleName" $ do
    it "returns module name from file path" $ do
      moduleName "src/Foo/Bar.hs" `shouldBe` ["Foo", "Bar"]
