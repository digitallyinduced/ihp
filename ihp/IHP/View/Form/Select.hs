{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE IncoherentInstances   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-|
Module: IHP.View.Form.Select
Description: Select and radio form controls
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.View.Form.Select where

import           GHC.Types
import           IHP.HSX.ConvertibleStrings ()
import           IHP.ModelSupport (InputValue, didTouchField, getModelName, inputValue, isNew)
import           IHP.Prelude
import           IHP.ValidationSupport
import           IHP.View.Classes ()
import           IHP.View.Types
import           IHP.ViewSupport

-- | Select inputs require you to pass a list of possible values to select.
--
-- > formFor project [hsx|
-- >     {selectField #userId users}
-- > |]
--
-- In the example above the variable users contains all the possible option values for the select.
--
-- You also need to define a instance @CanSelect User@:
--
-- > instance CanSelect User where
-- >     -- Here we specify that the <option> value should contain a `Id User`
-- >     type SelectValue User = Id User
-- >     -- Here we specify how to transform the model into <option>-value
-- >     selectValue user = user.id
-- >     -- And here we specify the <option>-text
-- >     selectLabel user = user.name
--
-- Given the above example, the rendered form will look like this:
--
-- > <!-- Assuming: users = [User { id = 1, name = "Marc" }, User { id = 2, name = "Andreas" }] -->
-- > <form ...>
-- >     <select name="user_id">
-- >         <option value="1">Marc</option>
-- >         <option value="2">Andreas</option>
-- >     </select>
-- > </form>
--
-- If you want a certain value to be preselected, set the value in the controller. For example, to have the first user be preselected in the above example:
--
-- > action NewProjectAction = do
-- >     users <- query @User |> fetch
-- >     let userId = headMay users |> maybe def (.id)
-- >     let target = newRecord @Project |> set #userId userId
-- >     render NewView { .. }
selectField :: forall fieldName model item.
    ( ?formContext :: FormContext model
    , HasField fieldName model (SelectValue item)
    , HasField "meta" model MetaBag
    , KnownSymbol fieldName
    , KnownSymbol (GetModelName model)
    , CanSelect item
    , InputValue (SelectValue item)
    , Typeable model
    , Eq (SelectValue item)
    , FieldBit fieldName model
    ) => Proxy fieldName -> [item] -> FormField
selectField field items = FormField
        { fieldType =
            let
                itemToTuple :: item -> (Text, Text)
                itemToTuple item = (selectLabel item, inputValue (selectValue item))
            in
                 SelectInput (map itemToTuple items)
        , fieldName = cs fieldName
        , fieldLabel = removeIdSuffix $ fieldNameToFieldLabel (cs fieldName)
        -- If the field is not touched, we don't want to render the value from the model
        -- so we force the user to select. If a value was explicitely set in the model, we
        -- render that value.
        , fieldValue = if IHP.ModelSupport.didTouchField field model || (not $ isNew model)
                    then inputValue (getField @fieldName model :: SelectValue item)
                    else ""
        , fieldInputId = cs (lcfirst (getModelName @model) <> "_" <> cs fieldName)
        , validatorResult = getValidationViolation field model
        , fieldClass = ""
        , labelClass = ""
        , disabled = False
        , disableLabel = False
        , disableGroup = False
        , disableValidationResult = False
        , additionalAttributes = []
        , cssFramework = ?formContext.cssFramework
        , helpText = ""
        , placeholder = "Please select"
        , required = False
        , autofocus = False
    }
    where
        fieldName = symbolVal field
        FormContext { model } = ?formContext
{-# INLINE selectField #-}

-- | Radio require you to pass a list of possible values to select. We use the same mechanism as for for 'selectField'.
--
-- > formFor project [hsx|
-- >     {radioField #userId users}
-- > |]
--
-- In the example above the variable users contains all the possible option values for the radios.
--
-- You also need to define a instance @CanSelect User@:
--
-- > instance CanSelect User where
-- >     -- Here we specify that the <option> value should contain a `Id User`
-- >     type SelectValue User = Id User
-- >     -- Here we specify how to transform the model into <option>-value
-- >     selectValue user = user.id
-- >     -- And here we specify the <option>-text
-- >     selectLabel user = user.name
--
-- Given the above example, the rendered form will look like this (omitting classes for brevity):
--
-- > <!-- Assuming: users = [User { id = 1, name = "Marc" }, User { id = 2, name = "Andreas" }] -->
-- > <form ...>
-- >     <fieldset>
-- >         <div>
-- >           <input type="radio" id="option1" value="1"/>
-- >           <label for="option1">Marc</label>
-- >         </div>
-- >         <div>
-- >           <input type="radio" id="option2" value="2"/>
-- >           <label for="option2">Andreas</label>
-- >         </div>
-- >     </fieldset>
-- > </form>
--
-- If you want a certain value to be preselected, set the value in the controller. For example, to have the first user be preselected in the above example:
--
-- > action NewProjectAction = do
-- >     users <- query @User |> fetch
-- >     let userId = headMay users |> maybe def (.id)
-- >     let target = newRecord @Project |> set #userId userId
-- >     render NewView { .. }
radioField :: forall fieldName model item.
    ( ?formContext :: FormContext model
    , HasField fieldName model (SelectValue item)
    , HasField "meta" model MetaBag
    , KnownSymbol fieldName
    , KnownSymbol (GetModelName model)
    , CanSelect item
    , InputValue (SelectValue item)
    , Typeable model
    , Eq (SelectValue item)
    , FieldBit fieldName model
    ) => Proxy fieldName -> [item] -> FormField
radioField field items = (selectField field items)
    { fieldType =
        let
            itemToTuple :: item -> (Text, Text)
            itemToTuple item = (selectLabel item, inputValue (selectValue item))
        in
                RadioInput (map itemToTuple items)
    , placeholder = ""
    }
{-# INLINE radioField #-}

class CanSelect model where
    -- | Here we specify the type of the @<option>@ value, usually an @Id model@
    type SelectValue model :: GHC.Types.Type

    -- | Here we specify the <option>-text
    selectLabel :: model -> Text
    default selectLabel :: Show model => model -> Text
    selectLabel = tshow

    -- | Here we specify how to transform the model into @<option>@-value
    selectValue :: model -> SelectValue model
    default selectValue :: HasField "id" model (SelectValue model) => model -> SelectValue model
    selectValue = (.id)
