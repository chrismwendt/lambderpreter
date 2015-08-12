{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Unbound.LocallyNameless
import Control.Monad.Trans.Maybe
import Control.Monad
import Text.Parsec hiding ((<|>))
import Text.Parsec.ByteString
import Control.Applicative
import qualified Data.ByteString.Char8 as BS
import System.Console.Haskeline

data Term = Var (Name Term) | Fun (Bind (Name Term) Term) | App Term Term

$(derive [''Term])

instance Alpha Term

instance Show Term where
  show t = runFreshM $ show' t

show' :: Term -> FreshM String
show' (Var n) = return $ show n
show' (Fun b) = do
  (p, t) <- unbind b
  return $ "(" ++ show p ++ " -> " ++ show t ++ ")"
show' (App l r) = return $ "(" ++ show l ++ " " ++ show r ++ ")"

instance Subst Term Term where
 isvar (Var v) = Just (SubstName v)
 isvar _     = Nothing

nameP :: Parser (Name Term)
nameP = string2Name <$> many1 letter

term :: Parser Term
term = subterm False
  where
  subterm ps = foldr1 (<|>) $ map try
    [ (if ps then parens else id) $ chainl1 (subterm (not ps)) (whitespace *> pure App)
    , Var <$> nameP
    , optionalParens $ (\p t -> Fun (bind p t)) <$> nameP <* whitespace <* string "->" <* whitespace <*> term
    ]

optionalParens :: Parser a -> Parser a
optionalParens a = try (parens a) <|> a

parens :: Parser a -> Parser a
parens = between (string "(") (string ")")

whitespace :: Parser BS.ByteString
whitespace = BS.pack <$> many1 (satisfy (\c -> c == ' ' || c == '\n'))

done :: MonadPlus m => m a
done = mzero

step :: Term -> MaybeT FreshM Term
step (Var _) = done
step (Fun b) = do
  (p,t1) <- unbind b
  body <- step t1
  return $ Fun (bind p body)
step (App (Fun b) t2) = do
  (x,t1) <- unbind b
  return $ subst x t2 t1
step (App t1 t2) =
      App <$> step t1 <*> pure t2
  <|> App <$> pure t1 <*> step t2

tc :: (Monad m, Functor m) => (a -> MaybeT m a) -> (a -> m a)
tc f a = do
  ma' <- runMaybeT (f a)
  case ma' of
    Just a' -> tc f a'
    Nothing -> return a

eval :: Term -> Either String Term
eval x = case (fv x :: [Name Term]) of
  [] -> Right $ runFreshM (tc step x)
  fvs -> Left $ "Free variables " ++ (show fvs)

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop :: InputT IO ()
  loop = do
    minput <- getInputLine "> "
    case minput of
      Nothing -> return ()
      Just "quit" -> return ()
      Just input -> do
        let v = either show (either id show . eval) $ parse (term <* eof) "" (BS.pack input)
        outputStrLn v
        loop
