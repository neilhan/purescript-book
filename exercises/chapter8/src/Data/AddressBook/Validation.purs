module Data.AddressBook.Validation where

import Prelude

import Data.AddressBook (Address, Person, PhoneNumber, PhoneType, address, person, phoneNumber)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String (length)
import Data.String.Regex (Regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse)
import Data.Validation.Semigroup (V, invalid, toEither)


data PersonField = FName | LName | Street | City | State | AllPhones | Phone PhoneType

derive instance equalPersonField :: Eq PersonField

derive instance genericPersonField :: Generic PersonField _

instance showPersonField :: Show PersonField where
  show a = genericShow a

data ValidationError = ValidationError String PersonField

type Errors
  = Array ValidationError

nonEmpty :: PersonField -> String -> V Errors String
nonEmpty field ""     = 
    invalid [ 
        ValidationError 
            ("Field '" <> (show field) <> "' cannot be empty") 
            field 
        ]
nonEmpty _     value  = pure value

validatePhoneNumbers :: PersonField -> Array PhoneNumber -> V Errors (Array PhoneNumber)
validatePhoneNumbers field []      =
    invalid [ 
        ValidationError 
            ("Field '" <> (show field) <> "' must contain at least one value")
            field
        ]
validatePhoneNumbers _ phones  =
  traverse validatePhoneNumber phones

lengthIs :: PersonField -> Int -> String -> V Errors String
lengthIs field len value | length value /= len =
    invalid [ 
        ValidationError 
            ("Field '" <> (show field) <> "' must have length " <> show len)
            field
        ]
lengthIs _     _   value = pure value

phoneNumberRegex :: Regex
phoneNumberRegex = unsafeRegex "^\\d{3}-\\d{3}-\\d{4}$" noFlags

matches :: PersonField -> Regex -> String -> V Errors String
matches _     regex value | test regex value 
                          = pure value
matches field _     _     = 
    invalid [ 
        ValidationError 
            ("Field '" <> (show field) <> "' did not match the required format")
            field
        ]

validateAddress :: Address -> V Errors Address
validateAddress a =
  address <$> nonEmpty Street  a.street
          <*> nonEmpty City    a.city
          <*> lengthIs State   2 a.state

validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber pn =
  phoneNumber <$> pure pn."type"
              <*> matches (Phone pn.type) phoneNumberRegex pn.number

validatePerson :: Person -> V Errors Person
validatePerson p =
  person <$> nonEmpty FName p.firstName
         <*> nonEmpty LName p.lastName
         <*> validateAddress p.homeAddress
         <*> validatePhoneNumbers AllPhones p.phones

validatePerson' :: Person -> Either Errors Person
validatePerson' p = toEither $ validatePerson p
