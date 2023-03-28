{ name = "front"
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-web"
  , "bifunctors"
  , "console"
  , "effect"
  , "either"
  , "halogen"
  , "http-methods"
  , "prelude"
  , "strings"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
