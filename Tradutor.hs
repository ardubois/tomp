import System.IO.Unsafe
import Data.IORef

data Tree = Prog | Function | Stm

data Prog = Main [Stm] [Function]

data Function = Fun Type String [(Type,String)] [Stm]

data Stm = Atrib String Exp | If Exp [Stm] [Stm] | Do [Stm] Exp 
    | For Exp Exp Exp [Stm] | PragmaTrans [Stm] | PragmaT [(Access,String)] [Stm] 
     | PragmaOMP String [Stm] | Decl [(Type,String)]
    | CallS String [Exp] | Printf String [Exp] | Return Exp | Block [Stm]

data Access = R | W | RW
   deriving (Eq,Show)
data Type = Void | Int | Float |  Deref Int Type | User String
data Exp = Var String | Num Int | Call String [Exp] | Op String Exp Exp | Asterisc Exp | Addr Exp 

-------------------

toVanilla :: Prog -> Prog
toVanilla (Main stms fun) = Main (map vaStm stms)  (map vaFun fun)

vaFun :: Function -> Function
vaFun (Fun t s args stmts) = Fun t s args (map vaStm stmts)

vaStm :: Stm -> Stm
vaStm (PragmaTrans stms) = PragmaOMP "parallel" (
                              (Decl [(User "vtm_t","tm")]):
                              (Atrib "tm" (Call "vtm_init" [Var "NULL"])) :  
                              (stms  ++ [ CallS "vtm_finalize" [Addr (Var "tm")]])
                              ) 
vaStm (PragmaT la stms)  = Block (vanillaStm la stms)
vaStm (PragmaOMP n stm) = PragmaOMP n (map vaStm stm)
vaStm (If e s1 s2 ) = If e (map vaStm s1) (map vaStm s2)
vaStm (Do s e) = Do (map vaStm s) e
vaStm (For e1 e2 e3 s) = For e1 e2 e3 (map vaStm s)
vaStm r = r

vanillaStm :: [(Access,String)] -> [Stm] -> [Stm]
vanillaStm la stm = Decl [(User "vtm_data_set_t", "dta00"++ show incCont)] : 
                    Decl [(User "vtm_tx_t", "tx_00"++ show getCont)]: 
                    CallS "vtm_dataset_init" [Addr (Var ("dta00"++show getCont))]:
                    genDataSetPack la ( "dta00"++ show incCont) ++
		    (Atrib ("tx_00"++ show getCont) (Call "vtm_start" [Addr (Var ( "dta00"++show getCont))]):
		    genBoilerplate la :
                    genStms la stm) ++ [CallS "vtm_commit" [Addr (Var ("tx_00"++show getCont))]]

genDataSetPack :: [(Access,String)] -> String -> [Stm]
genDataSetPack [] dta = []
genDataSetPack ((a,v):xs) dta = CallS "vtm_dataset_pack" [Addr (Var dta), Var (show a), Addr (Var v)]
                                                         :genDataSetPack xs dta

genBoilerplate :: [(Access,String)] -> Stm
genBoilerplate la =  Decl (genVarDecl la) 

cont :: IORef Int
cont = unsafePerformIO (newIORef 0)

incCont :: Int
incCont = unsafePerformIO $ do
                            x<-readIORef cont
                            writeIORef cont (x+1)
                            return (x+1)
incCont2 :: IO Int
incCont2 = do
          x<-readIORef cont
          writeIORef cont (x+1)
          return (x+1)

getCont :: Int
getCont = unsafePerformIO $readIORef cont
    

genVarDecl :: [(Access,String)] -> [(Type,String)]
genVarDecl []  = []
genVarDecl ((a,v):xs) = (User "vtm_uint_t", genVar v) : genVarDecl xs

genVar :: String -> String
genVar v = "tx_"++ v ++ "_tmp" ++ (show getCont)


genReads :: [String]-> [Stm]
genReads [] = []
genReads (v:xs) = Atrib (genVar v) (Call "vtm_read" [Addr (Var ( "tx_00"++show getCont)), Addr (Var v)])
                                    : genReads xs

getReadVars :: [(Access,String)]-> Exp -> [String]
getReadVars la (Var v)  
    | elem (R, v) la || elem (RW, v) la = [v]
    | otherwise =[]
getReadVars la (Num i) = []
getReadVars la (Call s e) = (foldr (++) [] . map (getReadVars la)) e
getReadVars la (Op s e1 e2) = getReadVars la e1 ++ getReadVars la e2
getReadVars la (Asterisc e) = getReadVars la e
getReadVars la (Addr e) = getReadVars la e

replaceReadVars :: [(Access,String)]-> Exp -> Exp
replaceReadVars la (Var v)  
    | elem (R, v) la || elem (RW, v) la =Var (genVar v)
    | otherwise = Var v
replaceReadVars la (Num i) = (Num i)
replaceReadVars la (Call s e) = Call s (map (replaceReadVars la) e)
replaceReadVars la (Op s e1 e2) = Op s (replaceReadVars la e1) (replaceReadVars la e2)
replaceReadVars la (Asterisc e) = Asterisc (replaceReadVars la e)
replaceReadVars la (Addr e) = Addr (replaceReadVars la e)

genStms  :: [(Access,String)] -> [Stm] -> [Stm]
genStms la [] = []
genStms la ((Atrib s exp):xs) = reads ++ ((Atrib (genVar s) expn) : write: genStms la xs)
	where
	vars = getReadVars la exp
	expn = replaceReadVars la exp
	reads = genReads vars
        write = CallS "vtm_write" [Addr (Var ( "tx_00"++show getCont)), Addr (Var (genVar s)), Addr (Var s)]


isTrans :: [(Access,String)] -> String -> Bool
isTrans [] v = False
isTrans ((a,v1):xs) v = v1 == v || isTrans xs v
-------------------
oset :: Int
oset = 3

strType :: Type -> String
strType Void = "void"
strType Int  = "int"
strType Float = "float"
strType (Deref 0 t) = strType t
strType (Deref n t) = "*" ++ strType (Deref (n-1) t)
strType (User s) = s

strAccess :: Access -> String
strAccess R = "R"
strAccess W = "W"
strAccess RW = "RW"

strExp :: Exp -> String
strExp (Var str) = str
strExp (Num i) = show i
strExp (Call fn e) = fn ++ "(" ++ strArgs e ++ ")"
strExp (Op o e1 e2) = strExp e1 ++ " " ++ o ++ " " ++ strExp e2 
strExp (Asterisc e) = "*" ++ strExp e
strExp (Addr e) = "&" ++ strExp e 

strArgs :: [Exp] -> String
strArgs [] = ""
strArgs [e] = strExp e
strArgs (e:es) = strExp e ++ ", " ++ strArgs es

strDecl :: [(Type,String)] -> String
strDecl [] = ""
strDecl [(t,s)] = strType t ++ " " ++ s ++ " "
strDecl ((t,s):es) = strType t ++ " " ++ s ++ "; " ++ strDecl es

strLAccess :: [(Access, String)]-> String
strLAccess [] = ""
strLAccess [(a,v)] = strAccess a ++ " : " ++ v ++ "  " 
strLAccess ((a,v):la) = strAccess a ++ " : " ++ v ++ ", " ++ strLAccess la

----------------------------------------------

pprint :: String -> IO ()
pprint = putStr

pprog :: Prog -> IO ()
pprog (Main stms funcs) = do
                      pprint "int main(){\n"
                      pprintStms 1 stms
                      pprint "}\n"
                      pprintFunctions funcs
pprintFunctions :: [Function] -> IO ()
pprintFunctions [] = return ()
pprintFunctions ((Fun t n decl stms):fs) = do  pprint ( strType t ++ " " ++ n ++ " (" ++ strDecl decl ++ " ){\n")
                                               pprintStms 1 stms
                                               pprint "}\n"
                                               pprintFunctions fs
                          

pprintStms :: Int -> [Stm] -> IO ()
pprintStms lv [] = return ()
pprintStms lv ((Atrib var exp):stms)  = do pprint $ gens (lv*oset) ++ var ++ " = " ++ strExp exp ++ ";\n" 
                                           pprintStms lv stms
pprintStms lv ((If e sts1 sts2 ):stms) =do pprint $ gens (lv*oset) ++ "if( "++  strExp e ++ " )\n"
                                           pprint $ gens (lv*oset) ++"{\n"
                                           pprintStms (lv+1) sts1
                                           pprint $ gens (lv*oset) ++"}\n"
                                           case sts2 of
                                              [] -> pprintStms lv stms
                                              _  -> do
                                                     pprint (gens (lv*oset) ++"else\n") 
                                                     pprint $ gens (lv*oset) ++"{\n"
                                                     pprintStms (lv+1) sts2
                                                     pprint $gens (lv*oset) ++ "}\n"
                                                     pprintStms lv stms
pprintStms lv ((PragmaTrans sts):stms) = do pprint $ gens (lv*oset) ++ "#pragma parallel transaction\n"
                                            pprint $ gens (lv*oset) ++ "{\n"
                                            pprintStms (lv+1) sts
                                            pprint $ gens (lv*oset) ++ "}\n"
                                            pprintStms lv stms
pprintStms lv ((PragmaT la sts):stms) =     do pprint $ gens (lv*oset) ++ "#pragma Transaction(" ++ strLAccess la ++")\n"
                                               pprint $ gens (lv*oset) ++ "{\n"
                                               pprintStms (lv+1) sts
                                               pprint $ gens (lv*oset) ++ "}\n"
                                               pprintStms lv stms
pprintStms lv ((PragmaOMP str sts):stms) = do pprint $ gens (lv*oset) ++ "#pragma opm " ++ str ++ "\n"
                                              pprint $ gens (lv*oset) ++ "{\n"
                                              pprintStms (lv+1) sts
                                              pprint $ gens (lv*oset) ++"}\n"
                                              pprintStms lv stms

pprintStms lv ((Decl l):stms) = do pprint $ gens (lv*oset) ++ strDecl l ++ ";\n" 
                                   pprintStms lv stms
pprintStms lv ((CallS n args):stms) = do pprint $ gens (lv*oset) ++ n ++"( " ++ strArgs args ++ ");\n"
                                         pprintStms lv stms
pprintStms lv ((Printf str exp):stms) = do pprint $ gens (lv*oset) ++ "printf(\"" ++ str ++"\"," ++  strArgs exp ++ ");\n"
                                           pprintStms lv stms
pprintStms lv ((Return exp):stms) = do pprint $ gens (lv*oset) ++ "return( " ++ strExp exp ++ " );\n"
                                       pprintStms lv stms
pprintStms lv ((Block l):stms) = do pprintStms lv l
                                    pprintStms lv stms
    

-- | Do [Stm] Exp 
--    | For Exp Exp Exp [Stm] | PragmaTrans [Stm] | PragmaT [Stm] 
--     | PragmaOMP String [Stm] | Decl [(Type,String)]
--    | CallS String [Exp] | Printf String [Exp] | Return Exp

--printProg :: Prog -> IO ()




gens :: Int -> String
gens 0 = ""
gens 1 = " "
gens n = " " ++ gens (n-1)

-------------------

prog1 :: Prog
prog1 = Main main1 [func1]

main1 = [ Decl [(Int,"n"),(Int,"r")], 
          PragmaTrans [ PragmaOMP "single" [ Atrib "r" (Call "fib" [Var "n"])]] ,
          Printf "fibo(%d) = %d\n" [Var "n", Var "r"] ] 

func1 = Fun Int "fib" [ (Int, "n")] [ Decl [(Int, "r")],
                                      If (Op "<" (Var "n") (Num 2))
                                        [ Atrib "r" (Var "n")]
                                        [ PragmaOMP "task"
                                             [ PragmaT [(RW , "r")]
                                                 [Atrib "r" (Op "+" (Var "r") (Call "fib" [Op "-" (Var "n") (Num 1) ] ) ) ]
                                             ],
                                          PragmaOMP "task"
                                             [ PragmaT [(RW , "r")]
                                                 [Atrib "r" (Op "+" (Var "r") (Call "fib" [Op "-" (Var "n") (Num 2) ] ) ) ]
                                             ]
                                        ],
                                       PragmaOMP "taskWait" [] ,
                                       Return (Var "r")
                                     ]


prog2 :: Prog
prog2 = Main main2 [func2]

main2  :: [Stm] 
main2 = [ Decl [(Int,"n"),(Int,"r")], 
          PragmaOMP "parallel" 
                         [ PragmaTrans 
                              [ PragmaOMP "single" [ Atrib "r" (Call "teste" [Var "n"])] ]
                         ], 
          Printf "teste(%d) = %d\n" [Var "n", Var "r"] ]

func2 = Fun Int "teste" [ (Int, "n")] [ Decl [(Int, "r")],
                                      If (Op "<" (Var "n") (Num 2))
                                        [ Atrib "r" (Var "n")]
                                        [ PragmaOMP "task"
                                             [ PragmaTrans
                                                 [Atrib "r" (Op "+" (Var "r") (Call "fib" [Op "-" (Var "n") (Num 1) ] ) ) ]
                                             ],
                                          PragmaOMP "task"
                                             [ PragmaTrans
                                                 [Atrib "r" (Op "+" (Var "r") (Call "fib" [Op "-" (Var "n") (Num 2) ] ) ) ]
                                             ]
                                        ],
                                       PragmaOMP "taskWait" [] ,
                                       Return (Var "r")
                                     ]


--pprint = print

--pprint o (Main

--Main := main { Stms }

--Function:= Type ID (ListArgs) { Stms }

--stm :=  x = E | if E Stms Stms? 
--      | do Stms while E 
--      | for E E E Stms 
--      | #pragma Transaction LV Stms
--      | #pragma omp parallel transaction cl

