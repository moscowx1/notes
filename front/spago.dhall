{ name = "front"
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-web"
  , "argonaut-codecs"
  , "argonaut-core"
  , "argonaut-generic"
  , "bifunctors"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "halogen"
  , "http-methods"
  , "maybe"
  , "media-types"
  , "prelude"
  , "strings"
  , "transformers"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
