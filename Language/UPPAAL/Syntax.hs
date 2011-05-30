{-# LANGUAGE DeriveFunctor #-}
module Language.UPPAAL.Syntax where

data Specification = Spec { specImports :: Maybe String
                          , specDeclarations :: [Declaration]
                          , specTemplates :: [Template]
                          , specInstantiation :: Maybe String
                          , specProcesses :: [(String,String,[Expression])]
                          , specSystem :: [String]
                          } deriving (Eq,Ord,Show)

data Positional a = Positional { position :: Maybe (Integer,Integer)
                               , value :: a
                               } deriving (Eq,Ord,Show,Functor)

noPos :: a -> Positional a
noPos = Positional Nothing

data Template = Template { templName :: Positional String
                         , templParameter :: Maybe (Positional [Parameter])
                         , templDeclaration :: [Declaration]
                         , templLocations :: [Positional Location]
                         , templInit :: Maybe String
                         , templTransitions :: [Positional Transition]
                         } deriving (Eq,Ord,Show)

data Location = Location { locId :: String
                         , locName :: Maybe (Positional String)
                         , locLabels :: [Positional Label]
                         , locUrgent :: Bool
                         , locCommited :: Bool
                         , locColor :: Maybe String
                         } deriving (Eq,Ord,Show)

data LabelKind = Invariant
               | Guard
               | Assignment
               | Synchronisation
               deriving (Eq,Ord,Show)

data Label = Label { lblKind :: LabelKind
                   , lblContent :: [Expression]
                   } deriving (Eq,Ord,Show)

data Transition = Transition { transId :: Maybe String
                             , transSource :: String
                             , transTarget :: String
                             , transLabel :: [Positional Label]
                             , transNails :: [(Integer,Integer)]
                             , transColor :: Maybe String
                             } deriving (Eq,Ord,Show)

data Declaration = VarDecl Type [(String,[ArrayDecl],Maybe Initialiser)]
                 | TypeDecl Type [(String,[ArrayDecl])]
                 | FunctionDecl Type String [Parameter] [Declaration] [Statement]
                 deriving (Eq,Ord,Show) -- Missing: Chan priorities

data TypePrefix = Urgent
                | Broadcast
                | Meta
                | Const
                deriving (Eq,Ord,Show)

data Type = Type { typePrefix :: Maybe TypePrefix
                 , typeId :: TypeId
                 } deriving (Eq,Ord,Show)

data TypeId = TypeName String
            | TypeInt (Maybe (Expression,Expression))
            | TypeClock
            | TypeChan
            | TypeBool
            | TypeScalar Expression
            | TypeStruct [(Type,String,[ArrayDecl])]
            deriving (Eq,Ord,Show)

data ArrayDecl = ExprArray Expression
               | TypeArray Type
               deriving (Eq,Ord,Show)

data ParameterConvention = CallByValue
                         | CallByReference
                         deriving (Eq,Ord,Show)

data Parameter = Parameter { paramType :: Type
                           , paramConvention :: ParameterConvention
                           , paramName :: String
                           , paramArray :: [ArrayDecl]
                           } deriving (Eq,Ord,Show)

data Expression = ExprId String
                | ExprNat Integer
                | ExprIndex Expression Expression
                | ExprPostIncr Expression
                | ExprPreIncr Expression
                | ExprPostDecr Expression
                | ExprPreDecr Expression
                | ExprAssign Assign Expression Expression
                | ExprUnary Unary Expression
                | ExprBinary Binary Expression Expression
                | ExprIf Expression Expression Expression
                | ExprMember Expression String
                | ExprForall String Type Expression
                | ExprExists String Type Expression
                | ExprDeadlock
                | ExprBool Bool
                deriving (Eq,Ord,Show)

data Assign = Assign
            | AssignPlus
            | AssignMinus
            | AssignMult
            | AssignDiv
            | AssignMod
            | AssignOr
            | AssignAnd
            | AssignXor
            | AssignShiftL
            | AssignShiftR
            deriving (Eq,Ord,Show)

data Unary = UnPlus
           | UnMinus
           | UnNot
           deriving (Eq,Ord,Show)

data Binary = BinLT
            | BinLTE
            | BinEq
            | BinNEq
            | BinGT
            | BinGTE
            | BinPlus
            | BinMinus
            | BinMult
            | BinDiv
            | BinMod
            | BinAnd
            | BinOr
            | BinXor
            | BinShiftL
            | BinShiftR
            | BinLAnd
            | BinLOr
            | BinMin
            | BinMax
            | BinImply
            deriving (Eq,Ord,Show)

data Initialiser = InitExpr Expression
                 | InitArray [Initialiser]
                 deriving (Eq,Ord,Show)

data Statement = Block [Declaration] [Statement]
               | Skip
               | StmtExpr Expression
               | ForLoop Expression Expression Expression Statement
               | Iteration String Type Statement
               | WhileLoop Expression Statement
               | DoWhile Statement Expression
               | StmtIf Expression Statement (Maybe Statement)
               | Return (Maybe Expression)
               deriving (Eq,Ord,Show)