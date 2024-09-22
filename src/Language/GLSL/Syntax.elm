module Language.GLSL.Syntax exposing
    ( CaseLabel(..)
    , Compound(..)
    , Condition(..)
    , Declaration(..)
    , Expr(..)
    , ExternalDeclaration(..)
    , Field(..)
    , FullType(..)
    , FunctionIdentifier(..)
    , FunctionPrototype(..)
    , InitDeclarator(..)
    , IntConstantKind(..)
    , InterpolationQualifier(..)
    , InvariantOrType(..)
    , InvariantQualifier(..)
    , LayoutQualifier(..)
    , LayoutQualifierId(..)
    , ParameterDeclaration(..)
    , ParameterQualifier(..)
    , ParameterTypeQualifier(..)
    , Parameters(..)
    , PrecisionQualifier(..)
    , Statement(..)
    , StorageQualifier(..)
    , StructDeclarator(..)
    , TranslationUnit(..)
    , TypeQualifier(..)
    , TypeSpecifier(..)
    , TypeSpecifierNoPrecision(..)
    , TypeSpecifierNonArray(..)
    )

{-| Reference: <https://hackage.haskell.org/package/language-glsl-0.3.0/docs/src/Language.GLSL.Syntax.html>
-}

-- TODO:
-- - add support for 'array of strings' ?
-- - add support for macro preprocessing
-- - add support for optional macro #include
-- - applicative style (see http://github.com/markusle/husky)?
-- - type checking
-- - check for constant expression where expected
-- - error reporting
-- - pretty-printing
-- - basic queries (inputs and outputs of the shader)
-- - support GLSL 1.40?
-- - proper testing (HUnit and QuickCheck)
-- - use hpc with the tests
-- - scoping
-- - clean module import/export
-- - order of Syntax data types and Pretty instances should be the same
-- - build with no warning
-- - use hlint
-- - push to github
-- - push to hackage
-- - use parsec 3
-- - handle all possible newlines (\n, \r, \r\n, \n\r)
-- - 80-columns clean
-- - lot of restriction of Samplers use (section 4.1.7),
-- well in fact, for plenty of things.
----------------------------------------------------------------------
-- Abstract syntax tree
----------------------------------------------------------------------


type TranslationUnit
    = TranslationUnit (List ExternalDeclaration)



-- at least one


type ExternalDeclaration
    = -- function declarations should be at top level (page 28)
      FunctionDeclaration FunctionPrototype
    | FunctionDefinition FunctionPrototype Compound
    | Declaration Declaration



-- TODO clean


type Declaration
    = -- e.g. layout (origin_upper_left) in vec4 gl_FragCoord;
      --      struct name { ... };
      --      struct name { ... } name;
      InitDeclaration InvariantOrType (List InitDeclarator)
    | Precision PrecisionQualifier TypeSpecifierNoPrecision
    | Block TypeQualifier String (List Field) (Maybe ( String, Maybe (Maybe Expr) )) -- constant expression
      -- e.g. layout (origin_upper_left) in; TODO check if it is only used for default layout.
    | TQ TypeQualifier



-- TODO regroup String (Maybe (Maybe Expr)) as Declarator and use it for
-- StructDeclarator.


type InitDeclarator
    = InitDecl String (Maybe (Maybe Expr)) (Maybe Expr) -- constant expression; assignment expression


type InvariantOrType
    = InvariantDeclarator
    | TypeDeclarator FullType


type FunctionPrototype
    = FuncProt FullType String (List ParameterDeclaration)


type ParameterDeclaration
    = ParameterDeclaration (Maybe ParameterTypeQualifier) (Maybe ParameterQualifier) TypeSpecifier (Maybe ( String, Maybe Expr ))



-- constant expression


type FullType
    = FullType (Maybe TypeQualifier) TypeSpecifier



-- sto
-- lay [sto]
-- int [sto]
-- inv [sto]
-- inv int sto


type TypeQualifier
    = TypeQualSto StorageQualifier
    | TypeQualLay LayoutQualifier (Maybe StorageQualifier)
    | TypeQualInt InterpolationQualifier (Maybe StorageQualifier)
    | TypeQualInv InvariantQualifier (Maybe StorageQualifier)
    | TypeQualInv3 InvariantQualifier InterpolationQualifier StorageQualifier


type TypeSpecifier
    = TypeSpec (Maybe PrecisionQualifier) TypeSpecifierNoPrecision


type InvariantQualifier
    = Invariant


type InterpolationQualifier
    = Smooth
    | Flat
    | NoPerspective


type LayoutQualifier
    = Layout (List LayoutQualifierId)


type LayoutQualifierId
    = LayoutQualId String (Maybe Expr) -- TODO Expr should be IntConstant


type Statement
    = -- declaration statement
      DeclarationStatement Declaration
      -- jump statement
    | Continue
    | Break
    | Return (Maybe Expr)
    | Discard -- fragment shader only
      -- compound statement
    | CompoundStatement Compound
      -- expression statement
    | ExpressionStatement (Maybe Expr)
      -- selection statement
    | SelectionStatement Expr Statement (Maybe Statement)
      -- switch statement
    | SwitchStatement Expr (List Statement)
    | CaseLabel CaseLabel
      -- iteration statement
    | While Condition Statement -- no new scope
    | DoWhile Statement Expr
    | For (Result (Maybe Expr) Declaration) (Maybe Condition) (Maybe Expr) Statement



-- 1st stmt: expression or declaration, 2nd: no new scope


type Compound
    = Compound (List Statement)


type Condition
    = Condition Expr
    | InitializedCondition FullType String Expr -- assignment expression


type CaseLabel
    = Case Expr
    | Default


type StorageQualifier
    = Const
    | Attribute -- vertex only
    | Varying
    | CentroidVarying
    | In
    | Out
    | CentroidIn
    | CentroidOut
    | Uniform


type TypeSpecifierNoPrecision
    = TypeSpecNoPrecision TypeSpecifierNonArray (Maybe (Maybe Expr)) -- constant expression


type TypeSpecifierNonArray
    = Void
    | Float
    | Int
    | UInt
    | Bool
    | Vec2
    | Vec3
    | Vec4
    | BVec2
    | BVec3
    | BVec4
    | IVec2
    | IVec3
    | IVec4
    | UVec2
    | UVec3
    | UVec4
    | Mat2
    | Mat3
    | Mat4
    | Mat2x2
    | Mat2x3
    | Mat2x4
    | Mat3x2
    | Mat3x3
    | Mat3x4
    | Mat4x2
    | Mat4x3
    | Mat4x4
    | Sampler1D
    | Sampler2D
    | Sampler3D
    | SamplerCube
    | Sampler1DShadow
    | Sampler2DShadow
    | SamplerCubeShadow
    | Sampler1DArray
    | Sampler2DArray
    | Sampler1DArrayShadow
    | Sampler2DArrayShadow
    | ISampler1D
    | ISampler2D
    | ISampler3D
    | ISamplerCube
    | ISampler1DArray
    | ISampler2DArray
    | USampler1D
    | USampler2D
    | USampler3D
    | USamplerCube
    | USampler1DArray
    | USampler2DArray
    | Sampler2DRect
    | Sampler2DRectShadow
    | ISampler2DRect
    | USampler2DRect
    | SamplerBuffer
    | ISamplerBuffer
    | USamplerBuffer
    | Sampler2DMS
    | ISampler2DMS
    | USampler2DMS
    | Sampler2DMSArray
    | ISampler2DMSArray
    | USampler2DMSArray
    | StructSpecifier (Maybe String) (List Field)
    | TypeName String -- TODO user-defined type, should verify if it is declared


type PrecisionQualifier
    = HighP
    | MediumP
    | LowP



-- TODO The type qualifier can be present only when there is one or more declarators.
-- There other restrictions, see 4.1.8.


type Field
    = Field (Maybe TypeQualifier) TypeSpecifier (List StructDeclarator)


type StructDeclarator
    = StructDeclarator String (Maybe (Maybe Expr)) -- constant expression


type Expr
    = -- primaryExpression
      Variable String
    | IntConstant IntConstantKind Int
    | FloatConstant Float
    | BoolConstant Bool
      -- postfixExpression
    | Bracket Expr Expr
    | FieldSelection Expr String
    | MethodCall Expr FunctionIdentifier Parameters
    | FunctionCall FunctionIdentifier Parameters
    | PostInc Expr
    | PostDec Expr
    | PreInc Expr
    | PreDec Expr
      -- unary expression
    | UnaryPlus Expr
    | UnaryNegate Expr
    | UnaryNot Expr
    | UnaryOneComplement Expr
      -- binary expression
    | Mul Expr Expr
    | Div Expr Expr
    | Mod Expr Expr
    | Add Expr Expr
    | Sub Expr Expr
    | LeftShift Expr Expr
    | RightShift Expr Expr
    | Lt Expr Expr
    | Gt Expr Expr
    | Lte Expr Expr
    | Gte Expr Expr
    | Equ Expr Expr
    | Neq Expr Expr
    | BitAnd Expr Expr
    | BitXor Expr Expr
    | BitOr Expr Expr
    | And Expr Expr
    | Or Expr Expr
    | Selection Expr Expr Expr -- ternary _ ? _ : _ operator
      -- assignment, the left Expr should be unary expression
    | Equal Expr Expr
    | MulAssign Expr Expr
    | DivAssign Expr Expr
    | ModAssign Expr Expr
    | AddAssign Expr Expr
    | SubAssign Expr Expr
    | LeftAssign Expr Expr
    | RightAssign Expr Expr
    | AndAssign Expr Expr
    | XorAssign Expr Expr
    | OrAssign Expr Expr
      -- sequence
    | Sequence Expr Expr


type IntConstantKind
    = Hexadecimal
    | Octal
    | Decimal


type Parameters
    = ParamVoid
    | Params (List Expr)


type ParameterQualifier
    = InParameter
    | OutParameter
    | InOutParameter


type ParameterTypeQualifier
    = ConstParameter


type FunctionIdentifier
    = -- TODO could be refine (I think a precision qualifier is not permitted,
      -- nor a complete struct definition)
      FuncIdTypeSpec TypeSpecifier
    | FuncId String
