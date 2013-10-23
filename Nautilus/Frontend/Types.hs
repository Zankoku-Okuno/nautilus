module Nautilus.Frontend.Types where

data Identifier = Id String
                | MangledId { sourceId :: String, mangledId :: String }
                | Anonymous

data Visibility = Intern | Opaque | Extern

data Type = NamedType Identifier 
          | CompType  Composite
          | EnumType  Identifier [(String, Int)]
          | PtrType   Type
          | RefType   Type
          | ArrType   (Either Integer Identifier)
          | DynArr    Type
          | FunType   Type [Type]
          | VoidType
          | PtrToAny
          | Tuple     [Type]

data Qualifier = Volatile | Shared | Static


data Composite = Struct Visibility Identifier [Field]
               | Union  Visibility Identifier [Field]
data Field = Field { fieldName :: Maybe Identifier
                   , fieldType :: Type
                   }

