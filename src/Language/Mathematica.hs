{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright: (c) 2022 Tom Westerhout
-- SPDX-License-Identifier: BSD-3-Clause
-- Maintainer: Tom Westerhout <14264576+twesterhout@users.noreply.github.com>
--
-- See README for more info
module Language.Mathematica
  ( someFunc,
    getWSTPLibrary,
    getWSTPDirectory,
    getWolframKernel,
    wsInitialize,
    wsDeinitialize,
  )
where

import Control.Monad (replicateM, when)
import Data.ByteString (packCStringLen, useAsCString, useAsCStringLen)
import Data.Int (Int64 (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Foreign.C.String (CString (..), withCString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr
import Foreign.Storable (Storable (..))
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import qualified System.Info
import System.Posix.DynamicLinker
import System.Process

someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)

newtype WolframKernel = WolframKernel {unWolframKernel :: FilePath}

newtype WSLibrary = WSLibrary {unWSLibrary :: DL}

type WSEnvironment = Ptr ()

type WSEnvironmentParameter = Ptr ()

type WSLink = Ptr ()

data PacketType
  = PKT_ILLEGAL
  | PKT_CALL
  | PKT_EVALUATE
  | PKT_RETURN
  | PKT_INPUTNAME
  | PKT_ENTERTEXT
  | PKT_ENTEREXPR
  | PKT_OUTPUTNAME
  | PKT_RETURNTEXT
  | PKT_RETURNEXPR
  | PKT_DISPLAY
  | PKT_DISPLAYEND
  | PKT_MESSAGE
  | PKT_TEXT
  | PKT_INPUT
  | PKT_INPUTSTR
  | PKT_MENU
  | PKT_SYNTAX
  | PKT_SUSPEND
  | PKT_RESUME
  | PKT_BEGINDLG
  | PKT_ENDDLG
  | PKT_FIRSTUSER
  | PKT_LASTUSER
  deriving stock (Show, Eq)

instance Enum PacketType where
  fromEnum x = case x of
    PKT_ILLEGAL -> 0
    PKT_CALL -> 7
    PKT_EVALUATE -> 13
    PKT_RETURN -> 3
    PKT_INPUTNAME -> 8
    PKT_ENTERTEXT -> 14
    PKT_ENTEREXPR -> 15
    PKT_OUTPUTNAME -> 9
    PKT_RETURNTEXT -> 4
    PKT_RETURNEXPR -> 16
    PKT_DISPLAY -> 11
    PKT_DISPLAYEND -> 12
    PKT_MESSAGE -> 5
    PKT_TEXT -> 2
    PKT_INPUT -> 1
    PKT_INPUTSTR -> 21
    PKT_MENU -> 6
    PKT_SYNTAX -> 10
    PKT_SUSPEND -> 17
    PKT_RESUME -> 18
    PKT_BEGINDLG -> 19
    PKT_ENDDLG -> 20
    PKT_FIRSTUSER -> 128
    PKT_LASTUSER -> 255
  toEnum x = case x of
    0 -> PKT_ILLEGAL
    7 -> PKT_CALL
    13 -> PKT_EVALUATE
    3 -> PKT_RETURN
    8 -> PKT_INPUTNAME
    14 -> PKT_ENTERTEXT
    15 -> PKT_ENTEREXPR
    9 -> PKT_OUTPUTNAME
    4 -> PKT_RETURNTEXT
    16 -> PKT_RETURNEXPR
    11 -> PKT_DISPLAY
    12 -> PKT_DISPLAYEND
    5 -> PKT_MESSAGE
    2 -> PKT_TEXT
    1 -> PKT_INPUT
    21 -> PKT_INPUTSTR
    6 -> PKT_MENU
    10 -> PKT_SYNTAX
    17 -> PKT_SUSPEND
    18 -> PKT_RESUME
    19 -> PKT_BEGINDLG
    20 -> PKT_ENDDLG
    128 -> PKT_FIRSTUSER
    255 -> PKT_LASTUSER
    _ -> error $ "invalid packet type: " <> show x

data TokenType
  = TK_ERR
  | TK_INT
  | TK_FUNC
  | TK_REAL
  | TK_STR
  | TK_SYM
  deriving stock (Show, Eq)

instance Enum TokenType where
  fromEnum x = case x of
    TK_ERR -> 0 -- /* bad token */
    TK_INT -> 43 --     /* '+' 43 0x2B 00101011 */
    TK_FUNC -> 70 -- /* 'F' 70 Ox46 01000110 */ /* non-leaf node */
    TK_REAL -> 42 --     /* '*' 42 0x2A 00101010 */
    TK_STR -> 32 -- /* '"' 34 0x22 00100010 */
    TK_SYM -> 35 -- /* '#' 35 0x23 # 00100011 */
  toEnum x = case x of
    0 -> TK_ERR
    43 -> TK_INT
    70 -> TK_FUNC
    42 -> TK_REAL
    32 -> TK_STR
    35 -> TK_SYM
    _ -> error $ "invalid token type: " <> show x

data WSExpression
  = WSInteger !Int
  | WSReal !Double
  | WSSymbol !Text
  | WSFunction !Text [WSExpression]
  deriving (Show)

-- int WSNewPacket(WSLINK link)
type WSNewPacket = WSLink -> IO CInt

foreign import ccall "dynamic"
  mkC_WSNewPacket :: FunPtr WSNewPacket -> WSNewPacket

wsNewPacket :: WSLibrary -> WSLink -> IO ()
wsNewPacket lib link = do
  fPtr <- dlsym (unWSLibrary lib) "WSNewPacket"
  ec <- mkC_WSNewPacket fPtr link
  when (ec == 0) $
    error $ "WSNewPacket failed with error code: " <> show ec

-- int WSNextPacket(WSLINK link)
type WSNextPacket = WSLink -> IO CInt

foreign import ccall "dynamic"
  mkC_WSNextPacket :: FunPtr WSNextPacket -> WSNextPacket

wsNextPacket :: WSLibrary -> WSLink -> IO PacketType
wsNextPacket lib link = do
  fPtr <- dlsym (unWSLibrary lib) "WSNextPacket"
  (toEnum . fromIntegral) <$> mkC_WSNextPacket fPtr link

-- int WSGetType(WSLINK link)
type WSGetTypeType = WSLink -> IO CInt

foreign import ccall "dynamic"
  mkC_WSGetType :: FunPtr WSGetTypeType -> WSGetTypeType

wsGetType :: WSLibrary -> WSLink -> IO TokenType
wsGetType lib link = do
  fPtr <- dlsym (unWSLibrary lib) "WSGetType"
  (toEnum . fromIntegral) <$> mkC_WSGetType fPtr link

-- int WSPutInteger64(WSLINK link,wsint64 i)
type WSPutInteger64 = WSLink -> Int64 -> IO CInt

foreign import ccall "dynamic"
  mkC_WSPutInteger64 :: FunPtr WSPutInteger64 -> WSPutInteger64

wsPutInteger64 :: WSLibrary -> WSLink -> Int64 -> IO ()
wsPutInteger64 lib link n = do
  fPtr <- dlsym (unWSLibrary lib) "WSPutInteger64"
  ec <- mkC_WSPutInteger64 fPtr link (fromIntegral n)
  when (ec == 0) $
    error $ "WSPutInteger64 failed with error code: " <> show ec

type WSGetInteger64 = WSLink -> Ptr Int64 -> IO CInt

foreign import ccall "dynamic"
  mkC_WSGetInteger64 :: FunPtr WSGetInteger64 -> WSGetInteger64

wsGetInteger64 :: WSLibrary -> WSLink -> IO Int64
wsGetInteger64 lib link = do
  fPtr <- dlsym (unWSLibrary lib) "WSGetInteger64"
  alloca $ \outPtr -> do
    ec <- mkC_WSGetInteger64 fPtr link outPtr
    when (ec == 0) $
      error $ "WSGetInteger64 failed with error code: " <> show ec
    peek outPtr

type WSPutReal64 = WSLink -> Double -> IO CInt

foreign import ccall "dynamic"
  mkC_WSPutReal64 :: FunPtr WSPutReal64 -> WSPutReal64

wsPutReal64 :: WSLibrary -> WSLink -> Double -> IO ()
wsPutReal64 lib link x = do
  fPtr <- dlsym (unWSLibrary lib) "WSPutReal64"
  ec <- mkC_WSPutReal64 fPtr link x
  when (ec == 0) $
    error $ "WSGetReal64 failed with error code: " <> show ec

type WSGetReal64 = WSLink -> Ptr Double -> IO CInt

foreign import ccall "dynamic"
  mkC_WSGetReal64 :: FunPtr WSGetReal64 -> WSGetReal64

wsGetReal64 :: WSLibrary -> WSLink -> IO Double
wsGetReal64 lib link = do
  fPtr <- dlsym (unWSLibrary lib) "WSGetReal64"
  alloca $ \outPtr -> do
    ec <- mkC_WSGetReal64 fPtr link outPtr
    when (ec == 0) $
      error $ "WSGetReal64 failed with error code: " <> show ec
    peek outPtr

-- int WSPutUTF8Function ( WSLINK mlp, const unsigned char *s, int length, int argn)
type WSPutUTF8Function = WSLink -> CString -> CInt -> CInt -> IO CInt

foreign import ccall "dynamic"
  mkC_WSPutUTF8Function :: FunPtr WSPutUTF8Function -> WSPutUTF8Function

wsPutUTF8Function :: WSLibrary -> WSLink -> Text -> Int -> IO ()
wsPutUTF8Function lib link funcName argCount = do
  fPtr <- dlsym (unWSLibrary lib) "WSPutUTF8Function"
  withText funcName $ \(c_funcName, numBytes) -> do
    ec <- mkC_WSPutUTF8Function fPtr link c_funcName (fromIntegral numBytes) (fromIntegral argCount)
    when (ec == 0) $
      error $ "WSPutUTF8Function failed with error code: " <> show ec

-- int WSGetFunction(WSLINK link,const char *s,int n)
type WSGetUTF8Function = WSLink -> Ptr CString -> Ptr CInt -> Ptr CInt -> IO CInt

foreign import ccall "dynamic"
  mkC_WSGetUTF8Function :: FunPtr WSGetUTF8Function -> WSGetUTF8Function

-- void WSReleaseUTF8Symbol(WSLINK link,const unsigned char *s,int len)
type WSReleaseUTF8Symbol = WSLink -> CString -> CInt -> IO ()

foreign import ccall "dynamic"
  mkC_WSReleaseUTF8Symbol :: FunPtr WSReleaseUTF8Symbol -> WSReleaseUTF8Symbol

wsGetUTF8Function :: WSLibrary -> WSLink -> IO (Text, Int)
wsGetUTF8Function lib link = do
  fPtr <- dlsym (unWSLibrary lib) "WSGetUTF8Function"
  alloca $ \c_strPtr ->
    alloca $ \c_numBytesPtr ->
      alloca $ \c_numArgsPtr -> do
        ec <- mkC_WSGetUTF8Function fPtr link c_strPtr c_numBytesPtr c_numArgsPtr
        when (ec == 0) $
          error $ "WSGetUTF8Function failed with error code: " <> show ec
        c_str <- peek c_strPtr
        c_numBytes <- peek c_numBytesPtr
        numArgs <- fromIntegral <$> peek c_numArgsPtr
        funcName <- decodeUtf8 <$> packCStringLen (c_str, fromIntegral c_numBytes)

        releasePtr <- dlsym (unWSLibrary lib) "WSReleaseUTF8Symbol"
        mkC_WSReleaseUTF8Symbol releasePtr link c_str c_numBytes
        pure (funcName, numArgs)

-- int WSEndPacket(WSLINK link)
type WSEndPacket = WSLink -> IO CInt

foreign import ccall "dynamic"
  mkC_WSEndPacket :: FunPtr WSEndPacket -> WSEndPacket

wsEndPacket :: WSLibrary -> WSLink -> IO ()
wsEndPacket lib link = do
  fPtr <- dlsym (unWSLibrary lib) "WSEndPacket"
  ec <- mkC_WSEndPacket fPtr link
  when (ec == 0) $
    error $ "WSEndPacket failed with error code: " <> show ec

withText :: Text -> ((CString, Int) -> IO a) -> IO a
withText str f = useAsCStringLen (encodeUtf8 str) f

type WSGetNext = WSLink -> IO CInt

foreign import ccall "dynamic"
  mkC_WSGetNext :: FunPtr WSGetNext -> WSGetNext

wsGetNext :: WSLibrary -> WSLink -> IO TokenType
wsGetNext lib link = do
  fPtr <- dlsym (unWSLibrary lib) "WSGetNext"
  (toEnum . fromIntegral) <$> mkC_WSGetNext fPtr link

putExpr :: WSLibrary -> WSLink -> WSExpression -> IO ()
putExpr lib link expr = go expr >> wsEndPacket lib link
  where
    go (WSInteger n) = wsPutInteger64 lib link (fromIntegral n)
    go (WSReal x) = wsPutReal64 lib link x
    go (WSFunction name args) = do
      wsPutUTF8Function lib link name (length args)
      mapM_ go args

getExpr :: WSLibrary -> WSLink -> IO WSExpression
getExpr lib link =
  wsNextPacket lib link >>= \case
    PKT_INPUTNAME -> wsNewPacket lib link >> getExpr lib link
    PKT_RETURN -> go
    packetType -> error $ "received packet: " <> show packetType
  where
    go = do
      tp <- wsGetNext lib link
      case tp of
        TK_INT -> WSInteger . fromIntegral <$> wsGetInteger64 lib link
        TK_REAL -> WSReal <$> wsGetReal64 lib link
        TK_FUNC -> do
          (funcName, argCount) <- wsGetUTF8Function lib link
          args <- replicateM argCount go
          pure $ WSFunction funcName args
        _ -> error $ "received token: " <> show tp

--     PKT_ILLEGAL    =   0
--
--     PKT_CALL       =   7
--     PKT_EVALUATE   =  13
--     PKT_RETURN     =   3
--
--     PKT_INPUTNAME  =   8
--     PKT_ENTERTEXT  =  14
--     PKT_ENTEREXPR  =  15
--     PKT_OUTPUTNAME =   9
--     PKT_RETURNTEXT =   4
--     PKT_RETURNEXPR =  16
--
--     PKT_DISPLAY    =  11
--     PKT_DISPLAYEND =  12
--
--     PKT_MESSAGE    =   5
--     PKT_TEXT       =   2
--
--     PKT_INPUT      =   1
--     PKT_INPUTSTR   =  21
--     PKT_MENU       =   6
--     PKT_SYNTAX     =  10
--
--     PKT_SUSPEND    =  17
--     PKT_RESUME     =  18
--
--     PKT_BEGINDLG   =  19
--     PKT_ENDDLG     =  20
--
--     PKT_FIRSTUSER  = 128
--     PKT_LASTUSER   = 255

getWSTPLibrary :: WolframKernel -> IO WSLibrary
getWSTPLibrary kernel = do
  libDir <- getWSTPDirectory kernel
  let libName
        | System.Info.os == "linux" = "libWSTP64i4.so"
        | System.Info.os == "darwin" = "libWSTP64i4.so"
        | otherwise = error $ "don't know the name of the library on " <> System.Info.os
  case System.Info.os of
    -- We need to pre-load libuuid.so on Linux
    "linux" -> do _ <- dlopen "libuuid.so" [RTLD_NOW, RTLD_GLOBAL]; pure ()
    _ -> pure ()
  WSLibrary <$> dlopen (libDir <> "/" <> libName) [RTLD_NOW, RTLD_LOCAL]

foreign import ccall "dynamic"
  mkC_WSInitialize :: FunPtr (WSEnvironmentParameter -> IO WSEnvironment) -> (WSEnvironmentParameter -> IO WSEnvironment)

wsInitialize :: WSLibrary -> IO WSEnvironment
wsInitialize lib = do
  fPtr <- dlsym (unWSLibrary lib) "WSInitialize"
  mkC_WSInitialize fPtr nullPtr

foreign import ccall "dynamic"
  mkC_WSDeinitialize :: FunPtr (WSEnvironment -> IO ()) -> (WSEnvironment -> IO ())

wsDeinitialize :: WSLibrary -> WSEnvironment -> IO ()
wsDeinitialize lib env = do
  fPtr <- dlsym (unWSLibrary lib) "WSDeinitialize"
  mkC_WSDeinitialize fPtr env

-- | WSLINK WSOpenString(WSENV env,const char *string,int *errno)
type WSOpenStringType = WSEnvironment -> CString -> Ptr CInt -> IO WSLink

foreign import ccall "dynamic"
  mkC_WSOpenString :: FunPtr WSOpenStringType -> WSOpenStringType

wsOpenString :: WSLibrary -> WSEnvironment -> String -> IO WSLink
wsOpenString lib env str = do
  fPtr <- dlsym (unWSLibrary lib) "WSOpenString"
  withCString str $ \c_str ->
    alloca $ \c_errorPtr -> do
      link <- mkC_WSOpenString fPtr env c_str c_errorPtr
      c_error <- peek c_errorPtr
      when (c_error /= 0 || link == nullPtr) $ error "WSOpenString failed"
      pure link

-- | void WSClose(WSLINK link)
type WSCloseType = WSLink -> IO ()

foreign import ccall "dynamic"
  mkC_WSClose :: FunPtr WSCloseType -> WSCloseType

wsClose :: WSLibrary -> WSLink -> IO ()
wsClose lib link = do
  fPtr <- dlsym (unWSLibrary lib) "WSClose"
  mkC_WSClose fPtr link

openDefaultLink :: WSLibrary -> WSEnvironment -> IO WSLink
openDefaultLink lib env = do
  kernel <- getWolframKernel
  wsOpenString lib env $
    "-linkname '\"" <> (unWolframKernel kernel) <> "\" -mathlink' -linkmode launch"

getWSTPDirectory :: WolframKernel -> IO FilePath
getWSTPDirectory kernel = do
  (status, stdout, stderr) <-
    readProcessWithExitCode
      (unWolframKernel kernel)
      [ "-noprompt",
        "-run",
        "WriteString[$Output,$InstallationDirectory <> \"/SystemFiles/Links/WSTP/DeveloperKit/\" <> $SystemID <> \"/CompilerAdditions\"];Exit[]"
      ]
      ""
  case status of
    ExitSuccess -> pure stdout
    ExitFailure ec ->
      let msg =
            mconcat $
              [ "'",
                unWolframKernel kernel,
                "' failed with exit code: ",
                show ec,
                if null stderr then "" else "\n" <> stderr
              ]
       in error msg

getWolframKernel :: IO WolframKernel
getWolframKernel =
  lookupEnv "HS_WOLFRAM_KERNEL" >>= \r -> case r of
    Just path -> pure $ WolframKernel path
    Nothing -> pure $ WolframKernel "WolframKernel"
