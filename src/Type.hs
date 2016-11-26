module Type where
data Type = Ptr Type | Arr Type| P_Int | P_Char | P_Void
    deriving (Eq)
instance Show Type where
    show P_Int = "int"
    show P_Char = "char"
    show P_Void = "void"
    show (Ptr t) = (show t) ++ "*"
    show (Arr t) = "[] of " ++ (show t)
instance Ord Type where
    P_Char <= P_Int = True
    P_Int <= (Ptr _) = True
    P_Char <= (Ptr _) = True
    (Arr t) <= (Ptr t2) = t <= t2
    (Ptr t) <= (Ptr t2) = t <= t2
    _ <= _ = False


toArr :: Type -> Type
toArr t = (Arr t)
toPtr :: Type -> Type
toPtr (Arr t) = (Ptr t)
toPtr t = error $ "Cannot convert " ++ show t  ++ " to pointer"

isArr :: Type -> Bool
isArr (Arr _) = True
isArr _ = False
isPtr :: Type -> Bool
isPtr (Ptr _) = True
isPtr _ = False

derefType :: Type -> Type
derefType (Ptr t) = t
derefType (Arr t) = t
derefType t = error $ "Cannot derefrence an expression of type " ++ (show t)

canAssign :: Type -> Type -> Bool
canAssign (P_Int) P_Char = True
canAssign P_Char P_Int = True
canAssign (Ptr P_Void) (Ptr _) = True
canAssign (Ptr _) (Ptr P_Void) = True
canAssign (Ptr t1) (Arr t2) = canAssign (Ptr t1) (Ptr t2)
canAssign t2 t1 = t2 == t1

sizeof :: Type -> Int
sizeof (P_Char) = 1
sizeof _ = 2


