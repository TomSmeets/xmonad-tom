module XMonad.Tom.XMobarHs
( Config    (..)
, XPosition (..)
, Align     (..)
, Border    (..)
, Command   (..)
, Run       (..)
, defaultConfig
, export
) where

import Data.List
import System.Directory
import System.FilePath

-- | The configuration data type
data Config =
    Config { font             :: String        -- ^ Font
           , additionalFonts  :: [String]      -- ^ List of alternative fonts
           , bgColor          :: String        -- ^ Backgroud color
           , fgColor          :: String        -- ^ Default font color
           , position         :: XPosition     -- ^ Top Bottom or Static
           , textOffset       :: Int           -- ^ Offset from top of window for text
           , iconOffset       :: Int           -- ^ Offset from top of window for icons
           , border           :: Border        -- ^ NoBorder TopB BottomB or FullB
           , borderColor      :: String        -- ^ Border color
           , borderWidth      :: Int           -- ^ Border width
           , alpha            :: Int           -- ^ Transparency from 0 (transparent) to 255 (opaque)
           , hideOnStart      :: Bool          -- ^ Hide (Unmap) the window on initialization
           , allDesktops      :: Bool          -- ^ Tell the WM to map to all desktops
           , overrideRedirect :: Bool          -- ^ Needed for dock behaviour in some non-tiling WMs
           , pickBroadest     :: Bool          -- ^ Use the broadest display instead of the first one by default
           , lowerOnStart     :: Bool          -- ^ lower to the bottom of the window stack on initialization
           , persistent       :: Bool          -- ^ Whether automatic hiding should be enabled or disabled
           , iconRoot         :: FilePath      -- ^ Root folder for icons
           , commands         :: [Run Command] -- ^ For setting the command, the command arguments and refresh rate for the programs to run (optional)
           , sepChar          :: String        -- ^ The character to be used for indicating commands in the output template (default '%')
           , alignSep         :: String        -- ^ Separators for left, center and right text alignment
           , template         :: String        -- ^ The output template
           } deriving Show

data XPosition = Top
               | TopW Align Int
               | TopSize Align Int Int
               | TopP Int Int
               | Bottom
               | BottomP Int Int
               | BottomW Align Int
               | BottomSize Align Int Int
               | Static {xpos, ypos, width, height :: Int}
               | OnScreen Int XPosition
                 deriving ( Read, Show, Eq )

data Align = L | R | C deriving ( Read, Show, Eq )

data Border = NoBorder
            | TopB
            | BottomB
            | FullB
            | TopBM Int
            | BottomBM Int
            | FullBM Int
              deriving ( Read, Show, Eq )

-- | The default configuration values
defaultConfig :: Config
defaultConfig =
    Config { font = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
           , additionalFonts = []
           , bgColor = "#000000"
           , fgColor = "#BFBFBF"
           , alpha   = 255
           , position = Top
           , border = NoBorder
           , borderColor = "#BFBFBF"
           , borderWidth = 1
           , textOffset = -1
           , iconOffset = -1
           , hideOnStart = False
           , lowerOnStart = True
           , persistent = False
           , allDesktops = True
           , overrideRedirect = True
           , pickBroadest = False
           , iconRoot = "."
           , commands = [ Run $ Date "%a %b %_d %Y * %H:%M:%S" "theDate" 10
                        , Run StdinReader]
           , sepChar = "%"
           , alignSep = "}{"
           , template = "%StdinReader% }{ " ++
                        "<fc=#00FF00>%uname%</fc> * <fc=#FF0000>%theDate%</fc>"
           }

cfgPairs :: [(String, (Config -> String))]
cfgPairs =  [ ("font"              , show.font)
            , ("additionalFonts"   , show.additionalFonts)
            , ("bgColor"           , show.bgColor)
            , ("fgColor"           , show.fgColor)
            , ("position"          , show.position)
            , ("textOffset"        , show.textOffset)
            , ("iconOffset"        , show.iconOffset)
            , ("border"            , show.border)
            , ("borderColor"       , show.borderColor)
            , ("borderWidth"       , show.borderWidth)
            , ("alpha"             , show.alpha)
            , ("hideOnStart"       , show.hideOnStart)
            , ("allDesktops"       , show.allDesktops)
            , ("overrideRedirect " , show.overrideRedirect )
            , ("pickBroadest"      , show.pickBroadest)
            , ("lowerOnStart"      , show.lowerOnStart)
            , ("persistent"        , show.persistent)
            , ("iconRoot"          , show.iconRoot)
            , ("commands"          , show.commands)
            , ("sepChar"           , show.sepChar)
            , ("alignSep"          , show.alignSep)
            , ("template"          , show.template)
            ]


-- Must be rewritten, to remove unnecessary defaults
-- instance Show Config where
--   showsPrec _ n s = "Config {" ++ e n ++ "}" ++ s
--     where e x = intercalate ", " [g (fst y) | y <- f x, uncurry (/=) y]
--           f x = zip (h x) (h defaultConfig)
--           g x = fst x ++ " = " ++ snd x
--           h x = map (\y -> (fst y, snd y x)) cfgPairs

data Command = Battery                               [String] Int
             | BatteryP           [String]           [String] Int
             | Brightness         [String]                    Int
             | BufferedPipeReader String [(Int, Bool, String)]
             | Com                String [String] String      Int
             | CommandReader      String String
             | CoreTemp                              [String] Int
             | Cpu                                   [String] Int
             | CpuFreq                               [String] Int
             | Date               String String               Int
             | DateZone           String String String String Int
             | DiskIO             [(String, String)] [String] Int
             | DiskU              [(String, String)] [String] Int
             | DynNetwork                            [String] Int
             | Kbd                [(String, String)]
             | Locks
             | MPD                                   [String] Int
             | Mail               [(String, String)] String
             | Mbox               [(String, String, String)] [String] String
             | Memory                                [String] Int
             | Mpris1             String             [String] Int
             | Mpris2             String             [String] Int
             | MultiCpu                              [String] Int
             | NamedXPropertyLog  String String
             | Network            String             [String] Int
             | PipeReader         String String
             | StdinReader
             | Swap                                  [String] Int
             | Thermal            String             [String] Int
             | ThermalZone        Int                [String] Int
             | TopMem                                [String] Int
             | TopProc                               [String] Int
             | Uptime                                [String] Int
             | Volume             String String      [String] Int
             | Weather            String             [String] Int
             | Wireless           String             [String] Int
             | XMonadLog
             | XPropertyLog       String
             deriving Show

data Run a = Run a

-- Must be rewritten, as derived version inserts parens
instance Show a => Show (Run a) where
  showsPrec _ (Run x) s = "Run " ++ show x ++ s

-- Example of use:
-- main = export $ config { ... }
-- remember to put a "$" after every "Run"
-- otherwise, configure it the same as you would a normal .xmobarrc
export :: Config -> IO ()
export conf = do
    home <- getHomeDirectory
    writeFile (home </> ".xmobarrc") (show conf)
