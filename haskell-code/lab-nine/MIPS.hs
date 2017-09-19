module MIPS where

import Text.Printf

type Code = [Instr]
type REG = String
type Label = String

data Instr =
  HALT
  | LI(REG, Int)
  | MOVE(REG, REG)
  | OP(REG, REG, BinOp, REG)
  | UOP(REG, UnaryOp, REG)
  | OPI(REG, REG, BinOp, Int)
  | LABEL(Label)
  | JUMP(Label)
  | BRANCH(REG, CmpOp, REG, Label)
  | LOAD(REG, REG, Int)
  | STORE(REG, REG, Int)
  deriving (Show, Eq)

data BinOp = And | Or deriving (Eq)
data CmpOp = Eq | Neq | Lt | Gte deriving (Eq)
data UnaryOp = Not deriving (Eq)

instance Show BinOp where
  show And = "and"
  show Or = "or"

instance Show CmpOp where
  show Eq = "beq"
  show Neq = "bneq"
  show Lt = "blt"
  show Gte = "bge"

instance Show UnaryOp where
  show Not = "not"

printInst inst =
  case inst of
    HALT -> printf "%s" "HALT"
    LI (dst, src) -> printf "li %s, %d" dst src
    MOVE (dst, src) -> printf "move %s, %s" dst src
    OP (dst, src1, op, src2) -> printf "%s %s, %s, %s" (show op) dst src1 src2
    UOP (dst, op, src1) -> printf "%s %s, %s" (show op) dst src1
    OPI (dst, src1, op, src2) -> printf "%s %s, %s, %d" (show op) dst src1 src2
    LABEL (label) -> printf "%s:" label
    JUMP (label) -> printf "jump %s" label
    BRANCH (src1, cmpop, src2, label) -> printf "%s %s, %s, %s" (show cmpop) src1 src2 label
    LOAD (dst, src, idx) -> printf "load %s, %s, %d" dst src idx
    STORE (src, dst, idx) -> printf "store %s, %s, %d" src dst idx

printInsts :: [Instr] -> IO ()
printInsts = mapM_ (putStrLn . printInst)
