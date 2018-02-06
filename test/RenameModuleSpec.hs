module RenameModuleSpec (spec) where

import           Test.Hspec
import           Test.Mockery.Directory

import           RenameModule

moduleNamed :: String -> String
moduleNamed name = unlines [
    "module " ++ name ++ " where"
  , "fib n = .."
  ]

spec :: Spec
spec = do
  describe "rename" $ around_ inTempDirectory $ do
    before_ ( do
      touch "src/Foo.hs"
      writeFile "src/Foo.hs" (moduleNamed "Foo")
      ) $ do
      it "renames a Haskell module" $ do
        rename "src/Foo.hs" "src/Bar.hs"
        readFile "src/Bar.hs" `shouldReturn` moduleNamed "Bar"

      it "moves a Haskell module to a directory" $ do
        touch "src/Bar/.placeholder"
        rename "src/Foo.hs" "src/Bar/"
        readFile "src/Bar/Foo.hs" `shouldReturn` moduleNamed "Bar.Foo"

      context "when module has a spec" $ do
        it "renames the spec" $ do
          touch "test/FooSpec.hs"
          writeFile "test/FooSpec.hs" (moduleNamed "FooSpec")
          rename "src/Foo.hs" "src/Bar.hs"
          readFile "test/BarSpec.hs" `shouldReturn` moduleNamed "BarSpec"

  describe "renameModule" $ do
    it "renames a Haskell module" $ do
      renameModule ["Foo"] ["Bar"] (moduleNamed "Foo") `shouldBe` moduleNamed "Bar"

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
