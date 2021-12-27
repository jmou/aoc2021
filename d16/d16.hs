{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
-- RIO supplies its own prelude
{-# LANGUAGE NoImplicitPrelude #-}

import qualified Data.ByteString.Lazy.Char8 as C
import RIO
import RIO.List (maximumMaybe, minimumMaybe)
import RIO.Vector ((!?))
import qualified RIO.Vector as V

-- Typeable is standard for runtime exception reflection, but unused here
data AppException = NonHex | ShortParse | BadOp | NonZeroPadding | BadEval deriving (Show, Typeable)

instance Exception AppException

-- Display is a more performance Show (also without the implied parity with Read)
instance Display AppException where display = displayShow

-- Either is used like Rust Result
-- the error is first because only the second type parameter can be used by Functor
type Result a = Either AppException a

data Bit = One | Zero deriving (Show, Eq)

instance Display Bit where
  display One = "1"
  display Zero = "0"

instance Display (Vector Bit) where
  -- fold is a generalized mconcat for Foldable
  -- foldMap is equivalent to fold . map
  display = foldMap display

data PolyOpKind = Sum | Product | Min | Max deriving (Show)

data BinOpKind = Greater | Less | Equal deriving (Show)

data PacketKind = Literal Integer | PolyOp PolyOpKind [Packet] | BinOp BinOpKind Packet Packet deriving (Show)

data Packet = Packet Word8 PacketKind deriving (Show)

instance Display PolyOpKind where display = displayShow

instance Display BinOpKind where
  display Greater = ">"
  display Less = "<"
  display Equal = "="

instance Display [Packet] where
  -- <> mappend monoidal append
  display packets = "[" <> go packets <> "]"
    where
      -- Monoid differ from Semigroup by also defining mempty (in this case equivalent to "")
      go [] = mempty
      go [p] = display p
      go (p : ps) = display p <> ", " <> go ps

instance Display PacketKind where
  display (Literal n) = display n
  display (PolyOp op packets) = "(" <> display op <> " " <> display packets <> ")"
  display (BinOp op packet1 packet2) = "(" <> display packet1 <> " " <> display op <> " " <> display packet2 <> ")"

instance Display Packet where
  display (Packet version kind) = "v" <> display version <> ":" <> display kind

hexToBit :: LByteString -> Maybe (Vector Bit)
-- fold by appending the next and accumulated Vectors, lifting over Maybe
-- liftA2 f a1 a2 == f <$> a1 <*> a2 == pure f <*> a1 <*> a2
-- pure embeds pure functions in an Applicative Functor
-- <*> ap (apply) "sequences" application
hexToBit = C.foldr (liftA2 (V.++) . (fmap V.fromList . charToBit)) (Just V.empty)
  where
    charToBit '0' = Just [Zero, Zero, Zero, Zero]
    charToBit '1' = Just [Zero, Zero, Zero, One]
    charToBit '2' = Just [Zero, Zero, One, Zero]
    charToBit '3' = Just [Zero, Zero, One, One]
    charToBit '4' = Just [Zero, One, Zero, Zero]
    charToBit '5' = Just [Zero, One, Zero, One]
    charToBit '6' = Just [Zero, One, One, Zero]
    charToBit '7' = Just [Zero, One, One, One]
    charToBit '8' = Just [One, Zero, Zero, Zero]
    charToBit '9' = Just [One, Zero, Zero, One]
    charToBit 'A' = Just [One, Zero, One, Zero]
    charToBit 'B' = Just [One, Zero, One, One]
    charToBit 'C' = Just [One, One, Zero, Zero]
    charToBit 'D' = Just [One, One, Zero, One]
    charToBit 'E' = Just [One, One, One, Zero]
    charToBit 'F' = Just [One, One, One, One]
    charToBit '\n' = Just []
    charToBit _ = Nothing

decodeInt :: Num p => Vector Bit -> p
decodeInt = V.foldl' (\n b -> 2 * n + case b of One -> 1; Zero -> 0) 0

-- >>> parseLiteral $ V.fromList [One,Zero,Zero,Zero,One,Zero,Zero,Zero,One,Zero,One]
-- Right ([One],18)
parseLiteral :: Vector Bit -> Result (Vector Bit, Integer)
parseLiteral bits = parseLiteral' bits 0
  where
    parseNibble bs = let (n, bs') = V.splitAt 4 bs in Right (bs', decodeInt n)
    parseLiteral' bs acc = case continuation !? 0 of
      Just Zero -> fmap (+ acc) <$> parseNibble bs'
      Just One -> do
        (bs'', n) <- parseNibble bs'
        parseLiteral' bs'' (16 * (n + acc)) -- 16 == 1 `shift` 4
      Nothing -> Left ShortParse
      where
        (continuation, bs') = V.splitAt 1 bs

polyOpKind :: Word8 -> Maybe PolyOpKind
polyOpKind 0 = Just Sum
polyOpKind 1 = Just Product
polyOpKind 2 = Just Min
polyOpKind 3 = Just Max
polyOpKind _ = Nothing

binOpKind :: Word8 -> Maybe BinOpKind
binOpKind 5 = Just Greater
binOpKind 6 = Just Less
binOpKind 7 = Just Equal
binOpKind _ = Nothing

parsePacket :: Vector Bit -> Result (Vector Bit, Packet)
parsePacket bits = fmap (Packet version) <$> parseKind content
  where
    (decodeInt -> version, (decodeInt -> typeid :: Word8, content)) = V.splitAt 3 <$> V.splitAt 3 bits
    parseKind = case typeid of
      -- nested fmaps lift over Result and (Vector Bit,)
      4 -> fmap (fmap Literal) . parseLiteral
      -- (<=<) "fish" or Kleisli composition is like function composition but monadic
      _ -> makeOp <=< uncurry parseSubpackets . V.splitAt 1
    makeOp (bs, packets) =
      (bs,) <$> case (polyOpKind typeid, binOpKind typeid, packets) of
        (Just op, Nothing, _ : _) -> Right $ PolyOp op packets
        (Nothing, Just op, [p1, p2]) -> Right $ BinOp op p1 p2
        _ -> Left BadOp
    parseSubpackets lengthType content' = case lengthType !? 0 of
      Just Zero -> uncurry parseByLen $ V.splitAt 15 content'
      Just One -> uncurry parseByNum $ V.splitAt 11 content'
      Nothing -> Left ShortParse
    parseWhile ::
      ([Result (Vector Bit, Packet)] -> [Result (Vector Bit, Packet)]) ->
      Vector Bit ->
      Result (Vector Bit, [Packet])
    parseWhile taker bs = collect bs . taker . unfold' parsePacket $ bs
    -- view pattern allows terse "parsing" of arguments
    parseByLen (decodeInt -> bitLength) bs =
      let (subpackets, bs') = V.splitAt bitLength bs
          -- take until the first error or no more bits to read
          takeRemaining = takeUntil (either (const True) (V.null . fst))
       in fmap ((bs',) . snd) . parseWhile takeRemaining $ subpackets
    parseByNum (decodeInt -> numPackets) bs = parseWhile (take numPackets) bs

-- cribbed from https://hackage.haskell.org/package/utility-ht-0.0.16/docs/Data-List-HT.html#v:takeUntil
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p = foldr (\x ys -> x : if p x then [] else ys) []

-- variant of unfoldr that exposes "seed" and uses Result
unfold' :: (b -> Result (b, a)) -> b -> [Result (b, a)]
unfold' f b0 =
  let go b = case f b of
        Right (b', a) -> Right (b', a) : go b'
        Left err -> [Left err]
   in go b0

-- >>> collect 0 [Right (1,9), Right (2,8)]
-- Right (2,[9,8])
collect :: b -> [Result (b, a)] -> Result (b, [a])
-- sequence of Traversable is quite general, but it evaluates Applicative (or Monadic) effects and collects the results
-- we use it to make a [Result] into Result [] (like Rust Vec<Result<...>>::collect)
-- reverse gets [a] in the right order after foldl'
collect b0 xs = fmap reverse . foldl' f (b0, []) <$> sequence xs
  where
    f (_, as) (b', a') = (b', a' : as)

-- the generalized form is significantly more complicated than these originals
-- decodeSubpackets :: Vector Bit -> Result [Packet]
-- decodeSubpackets bits =
--   if V.null bits
--     then Right []
--     else do
--       (bits', next) <- parsePacket bits
--       rest <- decodeSubpackets bits'
--       return $ next : rest
-- parseNumSubpackets num bits =
--   if num == 0
--     then Right (bits, [])
--     else do
--       (bits', next) <- parsePacket bits
--       rest <- parseNumSubpackets (num - 1) bits'
--       return $ (next :) <$> rest

decode :: Vector Bit -> Result Packet
decode bits = do
  (bits', packet) <- parsePacket bits
  if V.all (== Zero) bits' then return packet else Left NonZeroPadding

-- >>> utf8BuilderToText . display <$> hexToPacket "D2FE28"
-- Right "v6:2021"
-- >>> utf8BuilderToText . display <$> hexToPacket "38006F45291200"
-- Right "v1:(v6:10 < v2:20)"
-- >>> utf8BuilderToText . display <$> hexToPacket "EE00D40C823060"
-- Right "v7:(Max [v2:1, v4:2, v1:3])"
hexToPacket :: LByteString -> Result Packet
hexToPacket = decode <=< maybe (Left NonHex) Right . hexToBit

-- >>> versionSum <$> hexToPacket "8A004A801A8002F478"
-- Right 16
-- >>> versionSum <$> hexToPacket "620080001611562C8802118E34"
-- Right 12
-- >>> versionSum <$> hexToPacket "C0015000016115A2E0802F182340"
-- Right 23
-- >>> versionSum <$> hexToPacket "A0016C880162017C3686B18A3D4780"
-- Right 31
versionSum :: Packet -> Integer
versionSum (Packet version kind) =
  toInteger version + case kind of
    Literal _ -> 0
    PolyOp _ packets -> sum $ versionSum <$> packets
    BinOp _ packet1 packet2 -> versionSum packet1 + versionSum packet2

polyOpFn :: PolyOpKind -> [Integer] -> Maybe Integer
polyOpFn Sum = Just . sum
polyOpFn Product = Just . product
polyOpFn Min = minimumMaybe
polyOpFn Max = maximumMaybe

binOpFn :: BinOpKind -> Integer -> Integer -> Bool
binOpFn Greater = (>)
binOpFn Less = (<)
binOpFn Equal = (==)

-- >>> eval =<< hexToPacket "C200B40A82"
-- Right 3
-- >>> eval =<< hexToPacket "04005AC33890"
-- Right 54
-- >>> eval =<< hexToPacket "880086C3E88112"
-- Right 7
-- >>> eval =<< hexToPacket "CE00C43D881120"
-- Right 9
-- >>> eval =<< hexToPacket "D8005AC2A8F0"
-- Right 1
-- >>> eval =<< hexToPacket "F600BC2D8F"
-- Right 0
-- >>> eval =<< hexToPacket "9C005AC2F8F0"
-- Right 0
-- >>> eval =<< hexToPacket "9C0141080250320F1802104A08"
-- Right 1
eval :: Packet -> Result Integer
eval = maybe (Left BadEval) pure . eval'
  where
    eval' (Packet _ (Literal n)) = Just n
    -- mapM maps and collects the results; equivalent to sequence . map
    eval' (Packet _ (PolyOp op packets)) = polyOpFn op <=< mapM eval' $ packets
    -- Note: f <$> a1 <*> a2 == liftA2 f a1 a2
    eval' (Packet _ (BinOp op packet1 packet2)) = (toInt .) . binOpFn op <$> eval' packet1 <*> eval' packet2
    toInt b = if b then 1 else 0

main :: IO ()
main = withLazyFile "input" $ \input ->
  runSimpleApp $ do
    -- throwM throws the exception (not exactly sure how)
    -- throwString can be used without an Exception type
    packet <- either throwM pure $ hexToPacket input
    logInfo . display $ versionSum packet
    -- do <- desugars to >>= bind forward
    either throwM pure (eval packet)
      >>= logInfo . display
