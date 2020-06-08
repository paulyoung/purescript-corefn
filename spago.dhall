{ name = "corefn"
, dependencies =
  [ "foreign-generic"
  , "profunctor"
  , "spec-discovery"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
