module LunarMagic.Server.Context where

import LunarMagic.Web.Request (Request)
  
type ContextR ext =
  ( request :: Request
  | ext 
  )

type Context ext = Record (ContextR ext)