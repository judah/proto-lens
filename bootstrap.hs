-- Copyright 2016 Google Inc. All Rights Reserved.
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

-- To regenerate the bootstrapping proto bindings:
-- $ runghc bootstrap.hs
-- Note: if this doesn't work, you may need to edit the "location" field in
-- stack-boostrap.yaml.
import Control.Applicative ((<$>))
import Control.Exception (bracket_)
import System.FilePath ((</>))
import System.Process (callProcess, readProcess)

protoRoot = "google/protobuf/src"
protoc = "protoc"
bootstrapModuleRoot = "proto-lens-protoc/app"
useBootstrappingYaml = "--stack-yaml=stack-bootstrap.yaml"

-- Change this to build with an older version of stack.
-- TODO: remove this after we can use stack v2 (#332).
stack = "stack"

-- This should match (or at least be API-compatible with) the value of bootstrapCommit
-- in stack-bootstrap.yaml.
bootstrapCommit = "master"

main = do
    -- 1. Temporarily replace the bootstrap proto bindings in proto-lens-protoc
    --    with an older version that's compatible with the bootstrap version of
    --    proto-lens.
    -- 2. Build proto-lens-protoc against the older proto-lens.
    -- 3. Use it to generate new versions of the bootstrap proto bindings,
    --    overwriting the previous versions.
    callProcess "git" ["checkout", bootstrapCommit, "--",
                       bootstrapModuleRoot </> "Proto"]
    [installRoot] <- lines <$> readProcess stack
                    [useBootstrappingYaml, "path", "--local-install-root"] ""
    let protocGenHaskell = installRoot </> "bin/proto-lens-protoc"
    callProcess stack [useBootstrappingYaml, "build", "proto-lens-protoc"]
    callProcess protoc $
        [ "--plugin=protoc-gen-haskell=" ++ protocGenHaskell
        , "--haskell_out=no-runtime:" ++ bootstrapModuleRoot
        , "--proto_path=" ++ protoRoot
        ]
        ++ map (protoRoot </>)
            [ "google/protobuf/descriptor.proto"
            , "google/protobuf/compiler/plugin.proto"
            ]
    -- Verify that the generated code compiles successfully.
    callProcess stack ["build", "proto-lens-protoc"]
