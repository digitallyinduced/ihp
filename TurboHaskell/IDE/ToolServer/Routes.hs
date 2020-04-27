module TurboHaskell.IDE.ToolServer.Routes where
import TurboHaskell.RouterPrelude
import TurboHaskell.IDE.ToolServer.Types

instance AutoRoute SchemaDesignerController where
    parseArgument = parseTextArgument

instance AutoRoute WebReplController where
    parseArgument = parseTextArgument