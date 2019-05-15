module Foundation.Controller.Context where

import Prelude
import Foundation.Controller.RequestContext
import Foundation.ModelSupport

class Context context where
    createContext :: (?requestContext :: RequestContext, ?modelContext :: ModelContext) => IO context

instance Context () where
	createContext = return ()