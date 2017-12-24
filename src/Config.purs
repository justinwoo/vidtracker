module Config where

type Config =
  { vidtracker :: VidtrackerConfig
  , icons :: IconsConfig
  }

type VidtrackerConfig =
  { dir :: String
  }

type IconsConfig =
  { queryUrl :: String
  }
