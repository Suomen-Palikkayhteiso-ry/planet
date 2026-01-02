{-# LANGUAGE OverloadedStrings #-}

module Styles (css) where

import Data.Text (Text)
import qualified Data.Text as T

-- Embedded CSS (Elegant & Minimal)
css :: Text
css = T.unlines
    [ "body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; line-height: 1.6; color: #333; margin: 0; padding: 0; background-color: #f9f9f9; }"
    , ".layout { display: flex; max-width: 1400px; margin: 0 auto; gap: 20px; }"
    
    -- Timeline Navigation
    , ".timeline { width: 180px; flex-shrink: 0; position: sticky; top: 14px; align-self: start; height: calc(100vh - 40px); overflow-y: auto; padding-right: 10px; }"
    , ".timeline-header { font-weight: 900; font-size: 2rem; color: #eaeaea; margin-bottom: 20px; text-align: right; line-height: 1; }"
    , ".timeline ul { list-style: none; padding: 0; margin: 0; text-align: right; }"
    , ".timeline li { margin-bottom: 12px; }"
    , ".timeline a { text-decoration: none; color: #888; font-weight: 500; font-size: 0.95rem; transition: color 0.2s; display: block; padding: 4px 10px 4px 0; border-right: 2px solid transparent; }"
    , ".timeline a:hover { color: #111; border-right-color: #ddd; }"
    
    -- Main Content
    , ".main-content { flex-grow: 1; min-width: 0; }" -- min-width 0 prevents flex overflow
    , ".intro { }"
    , "h1 { font-weight: 700; color: #111; margin: 0; font-size: 2.5rem; }"
    , "h2.month-title { font-weight: 600; color: #444; border-bottom: 2px solid #333; padding-bottom: 10px; margin: 40px 0 20px 0; font-size: 1.5rem; }"
    
    -- Grid & Cards
    , ".grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(220px, 1fr)); gap: 20px; }"
    , ".card { background: white; border: 1px solid #eaeaea; padding: 20px; transition: box-shadow 0.2s; display: flex; flex-direction: column; position: relative; }"
    , ".card:hover { box-shadow: 0 4px 12px rgba(0,0,0,0.05); }"
    , ".card-content { display: flex; flex-direction: column; height: 100%; }"
    , ".source { font-size: 0.85rem; color: #666; text-transform: uppercase; letter-spacing: 0.5px; font-weight: 600; margin-bottom: 8px; display: block; }"
    , ".card-image { width: 100%; height: 180px; overflow: hidden; margin-bottom: 15px; border-radius: 4px; position: relative; }"
    , ".card-image img { width: 100%; height: 100%; object-fit: contain; display: none; }"
    , ".card-image img.loaded { display: block; }"
    , ".type-icon { background: rgba(0,0,0,0.1); color: #888; padding: 4px 6px; border-radius: 4px; font-size: 0.8rem; line-height: 1; }"
    , ".card h3 { margin: 0 0 10px 0; font-size: 1.1rem; line-height: 1.4; }"
    , ".card a { color: #111; text-decoration: none; }"
    , ".card a:hover { color: #0070f3; }"
    , ".date { font-size: 0.85rem; color: #888; }"
    , ".card-meta { display: flex; justify-content: space-between; align-items: center; margin-top: 15px; }"
    , ".description { font-size: 0.95rem; color: #555; margin: 0; overflow: hidden; flex-grow: 1; }"
    , ".description img { max-width: 100%; height: auto; display: none; }"
    , ".description img.loaded { display: block; margin: 10px 0; }"
    
    , "footer { margin-top: 80px; text-align: center; color: #888; font-size: 0.9rem; border-top: 1px solid #eaeaea; padding-top: 20px; }"
    
    -- Responsive
    , "@media (max-width: 800px) {"
    , "  .layout { flex-direction: column; gap: 20px; padding: 20px; }"
    , "  .timeline { width: 100%; height: auto; position: static; overflow-x: auto; padding-right: 0; border-bottom: 1px solid #eaeaea; padding-bottom: 10px; }"
    , "  .timeline-header { text-align: left; display: none; }"
    , "  .timeline ul { display: flex; text-align: left; gap: 15px; white-space: nowrap; }"
    , "  .timeline a { padding: 5px; border-right: none; border-bottom: 2px solid transparent; }"
    , "  .timeline a:hover { border-bottom-color: #333; }"
    , "}"
    , "@media (max-width: 600px) { .grid { grid-template-columns: 1fr; } }"

    -- Cookie Consent CSS
    , ".cookie-consent { position: fixed; bottom: 0; left: 0; right: 0; background: #222; color: white; padding: 20px; display: flex; justify-content: center; align-items: center; z-index: 1000; }"
    , ".cookie-consent.hidden { display: none; }"
    , ".consent-content { display: flex; align-items: center; gap: 20px; max-width: 1200px; flex-wrap: wrap; justify-content: center; }"
    , ".consent-content p { margin: 0; font-size: 0.9rem; }"
    
    -- Swapped Button Styles
    -- Consent: Primary (Blue)
    , "#consent-btn { background: #0070f3; color: white; border: none; padding: 10px 20px; border-radius: 5px; cursor: pointer; font-weight: 600; margin-left: 10px; }"
    , "#consent-btn:hover { background: #0051a2; }"
    -- Reject: Secondary (Transparent/White Border)
    , "#reject-btn { background: transparent; color: white; border: 1px solid white; padding: 10px 20px; border-radius: 5px; cursor: pointer; font-weight: 600; }"
    , "#reject-btn:hover { background: rgba(255,255,255,0.1); }"
    
    -- Revoke Button CSS
    , ".revoke-btn { position: fixed; bottom: 20px; right: 20px; background: rgba(255,255,255,0.8); border: 1px solid #ccc; border-radius: 50%; width: 40px; height: 40px; font-size: 20px; cursor: pointer; z-index: 999; display: flex; justify-content: center; align-items: center; box-shadow: 0 2px 5px rgba(0,0,0,0.1); }"
    , ".revoke-btn:hover { background: white; }"
    , ".revoke-btn.hidden { display: none; }"
    ]
