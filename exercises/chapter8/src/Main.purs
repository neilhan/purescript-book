module Main where

import Prelude

import Data.AddressBook (PhoneNumber, examplePerson)
import Data.AddressBook.Validation (Errors, PersonField(..), ValidationError(..), validatePerson')
import Data.Array (find, mapWithIndex, updateAt)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import React.Basic.DOM as D
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)
import React.Basic.Hooks (ReactComponent, element, reactComponent, useState)
import React.Basic.Hooks as R
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

-- Note that there's a Purty formatting bug that
-- adds an unwanted blank line
-- https://gitlab.com/joneshf/purty/issues/77
renderValidationErrors :: Errors -> Array R.JSX
renderValidationErrors [] = []
renderValidationErrors xs =
    [ D.div
        { children: (map renderError xs)
        }
    ]

renderError :: ValidationError -> R.JSX
renderError (ValidationError errMsg f) = 
    D.div { className: "alert alert-danger"
        , children: [ D.text errMsg ]
        }

renderIfError :: PersonField -> Errors -> R.JSX
renderIfError field errors =
    let
        maybeError = find (\(ValidationError msg f) -> f == field) errors
    in
        case maybeError of
            Nothing -> D.div_ []
            Just e -> renderError e

-- Helper function to render a single form field with an
-- event handler to update
formField :: String -> String -> PersonField -> String -> (String -> Effect Unit) -> Errors-> R.JSX
formField name placeholder field value setValue errors =
  D.label
    { className: "form-group row"
    , children:
        [ D.div
            { className: "col-sm col-form-label"
            , children: [ D.text name ]
            }
        , D.div
            { className: "col-sm"
            , children:
                [ D.input
                    { className: "form-control"
                    , placeholder
                    , value
                    , onChange:
                        let
                          handleValue :: Maybe String -> Effect Unit
                          handleValue (Just v) = setValue v
                          handleValue Nothing  = pure unit
                        in
                          handler targetValue handleValue
                    }
                ]
            }
        ]
        <> [renderIfError field errors]
    }

mkAddressBookApp :: Effect (ReactComponent {})
mkAddressBookApp =
  -- incoming \props are unused
  reactComponent "AddressBookApp" \props -> R.do
    -- `useState` takes a default initial value and returns the
    -- current value and a way to update the value.
    -- Consult react-hooks docs for a more detailed explanation of `useState`.
    Tuple person setPerson <- useState examplePerson
    let
      errors = case validatePerson' person of
        Left  e -> e
        Right _ -> []

      -- helper-function to return array unchanged instead of Nothing if index is out of bounds
      updateAt' :: forall a. Int -> a -> Array a -> Array a
      updateAt' i x xs = fromMaybe xs (updateAt i x xs)

      -- helper-function to render a single phone number at a given index
      renderPhoneNumber :: Errors -> Int -> PhoneNumber -> R.JSX
      renderPhoneNumber errors index phone =
        formField
            (show phone."type")
            "XXX-XXX-XXXX"
            (Phone phone.type)
            phone.number
            (\s -> setPerson _ { phones = updateAt' index phone { number = s } person.phones })
            errors

      -- helper-function to render all phone numbers
      renderPhoneNumbers :: Errors -> Array R.JSX
      renderPhoneNumbers errors = mapWithIndex (renderPhoneNumber errors) person.phones
    pure
      $ D.div
          { className: "container"
          , children:
                -- renderValidationErrors errors <> 
                [ D.div
                      { className: "row"
                      , children:
                          [ D.form_
                              $ [ D.h3_ [ D.text "Basic Information" ]
                                , formField "First Name" "First Name" FName person.firstName 
                                    (\s -> setPerson _ { firstName = s })
                                    errors
                                , formField "Last Name" "Last Name" LName person.lastName 
                                    (\s -> setPerson _ { lastName = s })
                                    errors
                                , D.h3_ [ D.text "Address" ]
                                , formField "Street" "Street" Street person.homeAddress.street 
                                    (\s -> setPerson _ { homeAddress { street = s } })
                                    errors
                                , formField "City" "City" City person.homeAddress.city 
                                    (\s -> setPerson _ { homeAddress { city = s } })
                                    errors
                                , formField "State" "State" State person.homeAddress.state 
                                    (\s -> setPerson _ { homeAddress { state = s } })
                                    errors
                                , D.h3_ [ D.text "Contact Information" ]
                                ]
                              <> renderPhoneNumbers errors
                          ]
                      }
                  ]
          }

main :: Effect Unit
main = do
  log "Rendering address book component"
  -- Get window object
  w <- window
  -- Get window's HTML document
  doc <- document w
  -- Get "container" element in HTML
  ctr <- getElementById "container" $ toNonElementParentNode doc
  case ctr of
    Nothing -> throw "Container element not found."
    Just c -> do
      -- Create AddressBook react component
      addressBookApp <- mkAddressBookApp
      let
        -- Create JSX node from react component. Pass-in empty props
        app = element addressBookApp {}
      -- Render AddressBook JSX node in DOM "container" element
      D.render app c
