#!/usr/bin/env runghc

-- ADS4All: Interactive Developer Story Demo
-- See why developers need authenticated data structures

import Data.List hiding (insert)
import Text.Printf
import Control.Monad

-- Simple BST for demo
data BST a = Empty | Node a (BST a) (BST a) deriving (Show, Eq)

insert :: Ord a => a -> BST a -> BST a
insert x Empty = Node x Empty Empty
insert x (Node y left right)
  | x < y     = Node y (insert x left) right
  | x > y     = Node y left (insert x right)
  | otherwise = Node y left right

-- Mock types for demo
type Hash = String
type Proof = [String]
data Auth mode a = Auth Hash a

-- Story demonstrations
demoMobileBanking :: IO ()
demoMobileBanking = do
  putStrLn "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "📱 STORY 1: Sarah's Mobile Banking in Kenya"
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  
  putStrLn "\n🌍 Location: Rural Kenya, 2G network"
  putStrLn "👤 User: Sarah, checking her M-Pesa balance"
  
  putStrLn "\n❌ WITHOUT ADS4All:"
  putStrLn "  Sarah: \"I need to check my balance\""
  putStrLn "  Phone: \"Downloading 1,247 transactions... (50MB)\""
  putStrLn "  [=====>........................] 23% (10 minutes remaining)"
  putStrLn "  Sarah: \"My data bundle is finished! 😢\""
  
  putStrLn "\n✅ WITH ADS4All:"
  putStrLn "  Sarah: \"I need to check my balance\""
  putStrLn "  Phone: \"Downloading proof... (640 bytes)\""
  putStrLn "  [==============================] 100% (0.3 seconds)"
  putStrLn "  Phone: \"✓ Balance: 5,420 KSH (Cryptographically verified)\""
  putStrLn "  Sarah: \"Perfect! And I still have data left! 😊\""
  
  putStrLn "\n💡 Impact: 99.99% less data, works on 2G, cryptographically secure"

demoMedicalPrivacy :: IO ()
demoMedicalPrivacy = do
  putStrLn "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "🏥 STORY 2: Dr. Chen's Hospital Privacy Challenge"
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  
  putStrLn "\n🏥 Location: Seattle General Hospital"
  putStrLn "📋 Task: Prove vaccination status to insurance"
  
  putStrLn "\n❌ WITHOUT ADS4All:"
  putStrLn "  Insurance: \"We need proof of COVID vaccination\""
  putStrLn "  Hospital: \"Here's the patient's full medical record\""
  putStrLn "  Records sent: [COVID vaccine ✓] [Mental health] [Genetics] [Everything!]"
  putStrLn "  Patient: \"You shared WHAT?! That's a HIPAA violation!\""
  putStrLn "  Hospital: \"Lawsuit incoming... 💸\""
  
  putStrLn "\n✅ WITH ADS4All:"
  putStrLn "  Insurance: \"We need proof of COVID vaccination\""  
  putStrLn "  Hospital: \"Here's a cryptographic proof for just that fact\""
  putStrLn "  Records sent: [COVID vaccine ✓ + 20 hash proof]"
  putStrLn "  Other records: [HIDDEN] [HIDDEN] [HIDDEN]"
  putStrLn "  Patient: \"Perfect! My privacy is protected!\""
  putStrLn "  Auditor: \"✓ HIPAA compliant, cryptographically verifiable\""
  
  putStrLn "\n💡 Impact: Selective disclosure, privacy preserved, legally compliant"

demoBlockchainFoodTruck :: IO ()
demoBlockchainFoodTruck = do
  putStrLn "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "🚚 STORY 3: Alex's Food Truck Accepts Crypto"
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  
  putStrLn "\n🌮 Location: Austin Food Truck"
  putStrLn "💰 Challenge: Accept Ethereum without $5000 server"
  
  putStrLn "\n❌ WITHOUT ADS4All:"
  putStrLn "  Customer: \"Can I pay with ETH?\""
  putStrLn "  Alex: \"Sure! Let me sync the blockchain...\""
  putStrLn "  Storage needed: 1.2 TB"
  putStrLn "  Sync time: 3 days"
  putStrLn "  Hardware cost: $5,000"
  putStrLn "  Alex: \"Actually, cash only! 😓\""
  
  putStrLn "\n✅ WITH ADS4All:"
  putStrLn "  Customer: \"Can I pay with ETH?\""
  putStrLn "  Alex: \"Sure! *opens phone app*\""
  putStrLn "  Storage needed: 10 KB"
  putStrLn "  Verification time: 0.5 seconds"
  putStrLn "  Hardware: Just a phone"
  putStrLn "  App: \"✓ Payment verified! (via cryptographic proof)\""
  putStrLn "  Alex: \"Tacos coming right up! 🌮\""
  
  putStrLn "\n💡 Impact: 1.2TB → 10KB, runs on phone, instant verification"

demoCDNIntegrity :: IO ()
demoCDNIntegrity = do
  putStrLn "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "🎬 STORY 4: StreamFlix Fights ISP Tampering"  
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  
  putStrLn "\n📺 Streaming: Latest episode of 'Dragon Throne'"
  putStrLn "😈 Threat: ISP injecting ads into video stream"
  
  putStrLn "\n❌ WITHOUT ADS4All:"
  putStrLn "  StreamFlix: \"Sending episode...\""
  putStrLn "  ISP: *secretly injects ads* 😈"
  putStrLn "  Viewer: \"Why are there random ads in my show?!\""
  putStrLn "  StreamFlix: \"We don't know what you mean...\""
  putStrLn "  Twitter: \"#StreamFlixScam trending\""
  
  putStrLn "\n✅ WITH ADS4All:"
  putStrLn "  StreamFlix: \"Episode root hash: 0xAB3F...29D1\""
  putStrLn "  ISP: *tries to inject ad*"
  putStrLn "  Player: \"⚠️ TAMPERING DETECTED! Hash mismatch!\""
  putStrLn "  Player: \"Switching to different CDN...\""
  putStrLn "  Viewer: \"Thanks for protecting my viewing experience!\""
  putStrLn "  StreamFlix: \"✓ Content integrity guaranteed\""
  
  putStrLn "\n💡 Impact: Tampering impossible, trust without central server"

demoGovernmentTransparency :: IO ()
demoGovernmentTransparency = do
  putStrLn "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "🗳️ STORY 5: Election Transparency in Estonia"
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  
  putStrLn "\n🇪🇪 Location: Estonian Election Commission"
  putStrLn "📊 Challenge: Prove voter registration without exposing database"
  
  putStrLn "\n❌ WITHOUT ADS4All:"
  putStrLn "  Citizen: \"Am I registered to vote?\""
  putStrLn "  Option 1: \"Trust us, you are!\" (No proof)"
  putStrLn "  Option 2: \"Here's the entire voter database!\" (Security disaster)"
  putStrLn "  Media: \"Election integrity questioned!\""
  putStrLn "  Hackers: \"Thanks for the database! 🎭\""
  
  putStrLn "\n✅ WITH ADS4All:"
  putStrLn "  Government: \"Public root hash: 0x7B4A...FF2C\""
  putStrLn "  Citizen: \"Am I registered?\""
  putStrLn "  System: \"Yes! Here's your cryptographic proof:\""
  putStrLn "  Proof: [Your record + 20 hashes = matches public root]"
  putStrLn "  Citizen: \"I can verify this independently! ✓\""
  putStrLn "  Observer: \"Election integrity mathematically proven!\""
  putStrLn "  Hacker: \"Nothing to steal, just hashes 😤\""
  
  putStrLn "\n💡 Impact: Public verification, zero data exposure, trust through math"

-- Main demo runner
main :: IO ()
main = do
  putStrLn "\n╔════════════════════════════════════════════════════════╗"
  putStrLn "║     🚀 ADS4All: Why Developers Need This Today 🚀     ║"
  putStrLn "║                                                        ║"
  putStrLn "║   'Don't Trust, Verify' - Now Possible Everywhere     ║"
  putStrLn "╚════════════════════════════════════════════════════════╝"
  
  putStrLn "\nPress Enter to see each story..."
  getLine
  
  demoMobileBanking
  putStrLn "\n[Press Enter for next story]"
  getLine
  
  demoMedicalPrivacy
  putStrLn "\n[Press Enter for next story]"
  getLine
  
  demoBlockchainFoodTruck
  putStrLn "\n[Press Enter for next story]"
  getLine
  
  demoCDNIntegrity
  putStrLn "\n[Press Enter for next story]"
  getLine
  
  demoGovernmentTransparency
  
  putStrLn "\n╔════════════════════════════════════════════════════════╗"
  putStrLn "║                  THE DEVELOPER PITCH                   ║"
  putStrLn "╠════════════════════════════════════════════════════════╣"
  putStrLn "║                                                        ║"
  putStrLn "║  Traditional:  data = server.getData()                ║"
  putStrLn "║                // Hope it's correct?                  ║"
  putStrLn "║                                                        ║"
  putStrLn "║  With ADS4All: (data, proof) = server.getData()       ║"
  putStrLn "║                assert verify(ROOT_HASH, data, proof)  ║"
  putStrLn "║                // KNOW it's correct!                  ║"
  putStrLn "║                                                        ║"
  putStrLn "╠════════════════════════════════════════════════════════╣"
  putStrLn "║  💡 One library, infinite trust                       ║"
  putStrLn "║  🔐 Cryptographic proof for any data structure        ║"
  putStrLn "║  ⚡ O(log n) proofs, constant-time verification       ║"
  putStrLn "║  🛡️ Type-safe: compiler prevents security mistakes    ║"
  putStrLn "║                                                        ║"
  putStrLn "║          \"HTTPS for your data structures\"             ║"
  putStrLn "╚════════════════════════════════════════════════════════╝"
  
  putStrLn "\n🎯 Ready to add cryptographic trust to your app?"
  putStrLn "   Get started: github.com/yourusername/ADS4All"
  putStrLn ""