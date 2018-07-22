module Config where

type Config =
  { vidtracker :: VidtrackerConfig
  }

type VidtrackerConfig =
  { dir :: String
  , exe :: String
  }
