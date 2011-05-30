{-# LANGUAGE FlexibleInstances #-}
module Language.UPPAAL.PrettyPrinter where

import Language.UPPAAL.Syntax
import Text.XML.HXT.Core (yes,withIndent)
import Text.XML.HXT.Arrow.Pickle
import Text.PrettyPrint

prettySpecification :: Specification -> String
prettySpecification = showPickled [withIndent yes]

instance XmlPickler Specification where
  xpickle = xpElem "nta" $ xpWrap (\(i,d,t,inst,s) -> Spec i (error "Can't parse declarations yet") t inst (error "Can't parse system processes yet") (error "Can't parse system yet"),
                                   \(Spec i d t inst p s) -> (i,case d of
                                                                 [] -> Nothing
                                                                 _ -> Just $ render $ prettyDecls d,
                                                              t,inst,render $ prettySystem p s))
            (xp5Tuple
             (xpOption $ xpElem "imports" xpText)
             (xpOption $ xpElem "declaration" xpText)
             (xpList1 xpickle)
             (xpOption $ xpElem "instantiation" xpText)
             (xpElem "system" xpText))
             
            

instance XmlPickler LabelKind where
  xpickle = xpWrapMaybe (\name -> case name of
                            "invariant" -> Just Invariant
                            "guard" -> Just Guard
                            "assignment" -> Just Assignment
                            "synchronisation" -> Just Synchronisation,
                         \val -> case val of
                           Invariant -> "invariant"
                           Guard -> "guard"
                           Assignment -> "assignment"
                           Synchronisation -> "synchronisation"
                         ) (xpTextAttr "kind")

positional :: String -> PU a -> PU (Positional a)
positional name el = xpWrap (\(p,c) -> Positional p c,\(Positional p c) -> (p,c))
                     (xpElem name $ xpPair (xpOption (xpPair (xpAttr "x" xpickle) (xpAttr "y" xpickle))) el)

instance XmlPickler (Positional Label) where
  xpickle = positional "label" $ xpWrap (\(x,y) -> Label x (error "Can't parse expressions yet"),
                                         \lbl -> (lblKind lbl,render $ prettyExprs $ lblContent lbl)) $
            (xpPair xpickle xpText)

instance XmlPickler (Positional Location) where
  xpickle = positional "location" $ xpWrap (\(i,c,n,l,u,com) -> Location i n l u com c,
                                            \(Location i n l u com c) -> (i,c,n,l,u,com)) $
            (xp6Tuple
             (xpTextAttr "id")
             (xpOption (xpTextAttr "color"))
             (xpOption $ positional "name" xpText)
             (xpList xpickle)
             (xpDefault False $ xpElem "urgent" $ xpLift True)
             (xpDefault False $ xpElem "commited" $ xpLift True))

instance XmlPickler (Positional Transition) where
  xpickle = positional "transition" $ xpWrap (\(i,c,s,t,l,n) -> Transition i s t l n c,
                                              \(Transition i s t l n c) -> (i,c,s,t,l,n))
            (xp6Tuple
             (xpOption $ xpTextAttr "id")
             (xpOption $ xpTextAttr "color")
             (xpElem "source" $ xpTextAttr "ref")
             (xpElem "target" $ xpTextAttr "ref")
             (xpList xpickle)
             (xpList (xpElem "nail" $ xpPair (xpAttr "x" xpickle) (xpAttr "y" xpickle))))

instance XmlPickler Template where
  xpickle = xpWrap (\(n,p,d,l,i,t) -> Template n (error "Can't parse parameters yet") (error "Can't parse declarations yet") l i t,
                    \(Template n p d l i t) -> (n,fmap (\p' -> fmap (\p'' -> render $ prettyParams p'') p') p,case d of
                                                   [] -> Nothing
                                                   _ -> Just $ render $ prettyDecls d,l,i,t))
            (xpElem "template" (xp6Tuple
                                (positional "name" xpText)
                                (xpOption $ positional "parameter" xpText)
                                (xpOption $ xpElem "declaration" xpText)
                                (xpList xpickle)
                                (xpOption $ xpElem "init" $ xpTextAttr "ref")
                                (xpList xpickle)))

prettyDecls :: [Declaration] -> Doc
prettyDecls decls = vcat (fmap prettyDecl decls)

prettyDecl :: Declaration -> Doc
prettyDecl (VarDecl tp vars) = prettyType tp <+> hsep (punctuate comma [ text var <> prettyArrayDecl arr <+> (case init of
                                                                                                                 Nothing -> empty
                                                                                                                 Just i -> char '=' <+> prettyInit i)
                                                                       | (var,arr,init) <- vars ]) <> semi
prettyDecl (TypeDecl tp alias) = text "typedef" <+> prettyType tp <+> hsep (punctuate comma [ text var <+> prettyArrayDecl arr
                                                                                            | (var,arr) <- alias ]) <> semi
prettyDecl (FunctionDecl tp name args decls body)
  = prettyType tp <+> text name <> parens (prettyParams args) <> (prettyStmt (Block decls body))

prettyParams :: [Parameter] -> Doc
prettyParams = hsep . punctuate comma . fmap prettyParam

prettyParam :: Parameter -> Doc
prettyParam p = prettyType (paramType p) <+> 
                (case paramConvention p of
                    CallByValue -> empty
                    CallByReference -> char '&') <>
                (text $ paramName p) <>
                (prettyArrayDecl $ paramArray p)                

prettyType :: Type -> Doc
prettyType tp = (case typePrefix tp of
                    Nothing -> empty
                    Just pre -> prettyTypePrefix pre) <+> prettyTypeId (typeId tp)

prettyTypePrefix :: TypePrefix -> Doc
prettyTypePrefix Urgent = text "urgent"
prettyTypePrefix Broadcast = text "broadcast"
prettyTypePrefix Meta = text "meta"
prettyTypePrefix Const = text "const"

prettyTypeId :: TypeId -> Doc
prettyTypeId (TypeName i) = text i
prettyTypeId (TypeInt mlim) = text "int" <> (case mlim of
                                                Nothing -> empty
                                                Just (l,u) -> brackets (prettyExpr 0 l <> comma <> prettyExpr 0 u))
prettyTypeId TypeClock = text "clock"
prettyTypeId TypeChan = text "chan"
prettyTypeId TypeBool = text "bool"
prettyTypeId (TypeScalar e) = text "scalar" <> brackets (prettyExpr 0 e)
prettyTypeId (TypeStruct cons) = text "struct" <+> braces (vcat [ prettyType tp <+> text name <> prettyArrayDecl arr <> semi | (tp,name,arr) <- cons ])

prettyArrayDecl :: [ArrayDecl] -> Doc
prettyArrayDecl [] = empty
prettyArrayDecl ((ExprArray e):xs) = brackets (prettyExpr 0 e) <> prettyArrayDecl xs
prettyArrayDecl ((TypeArray tp):xs) = brackets (prettyType tp) <> prettyArrayDecl xs

precCheck :: Int -> Int -> Doc -> Doc
precCheck lp p d = if p >= lp
                   then parens d
                   else d

prettyExprs :: [Expression] -> Doc
prettyExprs = hsep . punctuate comma . fmap (prettyExpr 0)

prettyExpr :: Int -> Expression -> Doc
prettyExpr _ (ExprId name) = text name
prettyExpr _ (ExprNat i) = integer i
prettyExpr p (ExprIndex e i) = precCheck 19 p $ (prettyExpr 19 e) <> brackets (prettyExpr 0 i)
prettyExpr p (ExprPostIncr e) = precCheck 18 p $ (prettyExpr 18 e) <> text "++"
prettyExpr p (ExprPreIncr e) = precCheck 18 p $ text "++" <> (prettyExpr 18 e)
prettyExpr p (ExprPostDecr e) = precCheck 18 p $ (prettyExpr 18 e) <> text "--"
prettyExpr p (ExprPreDecr e) = precCheck 18 p $ text "--" <> (prettyExpr 18 e)
prettyExpr p (ExprAssign ass l r) = precCheck 5 p $ (prettyExpr 5 l) <+> (text $ case ass of
                                                                             Assign -> "="
                                                                             AssignPlus -> "+="
                                                                             AssignMinus -> "-="
                                                                             AssignMult -> "*="
                                                                             AssignDiv -> "/="
                                                                             AssignMod -> "%="
                                                                             AssignOr -> "|="
                                                                             AssignAnd -> "&="
                                                                             AssignXor -> "^="
                                                                             AssignShiftL -> "<<="
                                                                             AssignShiftR -> ">>=") <+> (prettyExpr 5 r)
prettyExpr p (ExprUnary u e) = precCheck 18 p $ (char $ case u of
                                                    UnPlus -> '+'
                                                    UnMinus -> '-'
                                                    UnNot -> '!') <> (prettyExpr 18 e)
prettyExpr p (ExprBinary b l r)
  = let np = case b of
          BinLT -> 13
          BinLTE -> 13
          BinEq -> 12
          BinNEq -> 12
          BinGT -> 13
          BinGTE -> 13
          BinPlus -> 16
          BinMinus -> 16
          BinMult -> 17
          BinDiv -> 17
          BinMod -> 17
          BinAnd -> 11
          BinOr -> 9
          BinXor -> 10
          BinShiftL -> 15
          BinShiftR -> 15
          BinLAnd -> 8
          BinLOr -> 7
          BinMin -> 14
          BinMax -> 14
          BinImply -> 2
        op = text $ case b of
          BinLT -> "<"
          BinLTE -> "<="
          BinEq -> "=="
          BinNEq -> "!="
          BinGT -> ">"
          BinGTE -> ">="
          BinPlus -> "+"
          BinMinus -> "-"
          BinMult -> "*"
          BinDiv -> "/"
          BinMod -> "%"
          BinAnd -> "&"
          BinOr -> "|"
          BinXor -> "^"
          BinShiftL -> "<<"
          BinShiftR -> ">>"
          BinLAnd -> "&&"
          BinLOr -> "||"
          BinMin -> "<?"
          BinMax -> ">?"
          BinImply -> "imply"
    in precCheck np p $ (prettyExpr np l) <+> op <+> (prettyExpr np r)
prettyExpr p (ExprIf cond t f) = precCheck 6 p $ (prettyExpr 6 cond) <+> char '?' <+> prettyExpr 6 t <+> char ':' <+> prettyExpr 6 f
prettyExpr p (ExprMember e m) = precCheck 19 p $ (prettyExpr 19 e) <> char '.' <> text m
prettyExpr p (ExprForall var tp e) = precCheck 1 p $ text "forall" <+> parens (text var <+> colon <+> prettyType tp) <+> prettyExpr 1 e
prettyExpr p (ExprExists var tp e) = precCheck 1 p $ text "exists" <+> parens (text var <+> colon <+> prettyType tp) <+> prettyExpr 1 e
prettyExpr _ ExprDeadlock = text "deadlock"
prettyExpr _ (ExprBool x) = text $ if x then "true" else "false"

prettyStmt :: Statement -> Doc
prettyStmt (Block decls stmts) = braces $ nest 2 $ (vcat $ (fmap prettyDecl decls)++(fmap prettyStmt stmts))
prettyStmt Skip = semi
prettyStmt (StmtExpr e) = prettyExpr 0 e <> semi
prettyStmt (ForLoop i1 i2 i3 body) = text "for" <> parens ((prettyExpr 0 i1) <> semi <>
                                                           (prettyExpr 0 i2) <> semi <>
                                                           (prettyExpr 0 i3) <> semi) <+> prettyStmt body
prettyStmt (Iteration var tp body) = text "for" <> parens (text var <+> colon <+> prettyType tp) <+> prettyStmt body
prettyStmt (WhileLoop cond body) = text "while" <> parens (prettyExpr 0 cond) <+> prettyStmt body
prettyStmt (DoWhile body cond) = text "do" <+> prettyStmt body <+> text "while" <> parens (prettyExpr 0 cond)
prettyStmt (StmtIf cond t f) = text "if" <> parens (prettyExpr 0 cond) <+> prettyStmt t <+> (case f of
                                                                                                Just rf -> text "else" <+> prettyStmt rf
                                                                                                Nothing -> empty)
prettyStmt (Return e) = text "return" <+> (case e of
                                              Just re -> prettyExpr 0 re
                                              Nothing -> empty) <> semi

prettyInit :: Initialiser -> Doc
prettyInit (InitExpr e) = prettyExpr 0 e
prettyInit (InitArray arr) = braces (hsep $ punctuate comma (fmap prettyInit arr))

prettySystem :: [(String,String,[Expression])] -> [String] -> Doc
prettySystem procs sys = vcat $ [ text name <+> char '=' <+> text templ <> parens (prettyExprs args)
                                | (name,templ,args) <- procs ] ++
                         [ text "system" <+> hsep (punctuate comma (fmap text sys)) <> semi ]