module Game.Base where

import Poker.Base
import Poker.Parse.Base
import Poker.Filter.Eval.Base
import "poker-base" Poker.Base
import Poker.Filter.Eval.AST.Base
import Poker.Filter.Eval.Internal hiding (runQuery)
import qualified Poker.Filter.Eval.Internal as I
import Poker.Filter.Eval.Types
import Poker.Range (Range (..))
import Poker.Filter.Parser.Parser (parseQuery)
import System.Directory
import System.FilePath.Posix
import Data.Map (Map)
import Control.Monad
import Algebra.Lattice.Ordered (Ordered(Ordered))
import Data.Bifunctor (Bifunctor(second, first))
import Data.Functor.Foldable (Fix(Fix), Recursive(cata))

import Parse.Parser
import Control.Monad.State (StateT(StateT))
import Control.Monad.Except (ExceptT)
import Control.Monad.State.Lazy (runStateT)
import Control.Monad.Trans.Except (runExceptT)
import Poker.Game.Internal (prettyPrint)
import qualified Control.Lens as L
import Poker.Game.Bovada (IsBetSize, emulateAction, BovadaGame)
import Poker.Game.Bovada (GameErrorBundle(..))
import Poker.Game.Bovada (GameState(GameState))
import Control.Applicative (many)
import Poker.Game.Bovada (stateHandText)
import Control.Monad.State (evalStateT)
import Test.Tasty.HUnit (assertBool)
import Text.Printf (printf)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Debug.Trace (traceShowM)
import Poker.Game.Bovada (activeBet)
import Poker.Game.Bovada (toActQueue)
import Control.Monad.State (MonadState(get))
import Poker.Game.HasAvailableActions (HasAvailableActions(getAvailableActions))


unit_testEmulateHands :: IO ()
unit_testEmulateHands = do
  ress <- fmap (\(ix, h) -> checkSuccess ix h . test $ h) . zip [0..] <$> testHands
  sequence_ ress
  -- seq ress
  where
    -- checkSuccess :: (Show b, Show e, Show a) => Integer -> Hand b -> Either (GameErrorBundle e) a -> IO ()
    checkSuccess ix hand (Left e) = assertBool ("could not emulate hand no. " ++ show ix ++ ": " ++ _handText hand ++ show (_bundleError e)) False
    checkSuccess ix hand (Right a) = pure ()

test :: forall b. (Show b, IsBetSize b, Num b) => Hand b -> Either (GameErrorBundle b) ()
test hand =
  let initState :: GameState b = handToState hand
      actionsM :: [StateT (GameState b) (Either (GameErrorBundle b)) ()] = emulateAction <$> _handActions hand
  in evalStateT (sequence_ actionsM) initState

unit_availableActions :: IO ()
unit_availableActions = do
  hands <- testHands
  sequence_ $ testAvailableActions <$> zip [1..] hands
  where
    testAvailableActions :: (Integer, Hand BetSize) -> IO ()
    testAvailableActions (ix, hand) = do
      let initState = handToState hand
      let actionsM :: [StateT (GameState BetSize) (ExceptT (GameErrorBundle BetSize) IO) ()]
          actionsM = emulateWithCheck <$> _handActions hand
      runExceptT $ evalStateT (sequence actionsM) initState
      pure ()
      where
        emulateWithCheck :: (MonadIO m, BovadaGame BetSize m) => Action BetSize -> m ()
        emulateWithCheck act = do
          q <- L.use toActQueue
          availableActsOpt <- getAvailableActions -- traceShowM =<< L.use activeBet
          case availableActsOpt of
            Nothing -> pure () -- TODO assert rest acts are not players'
            Just (pos, availableActs) -> do
              -- Check non-available actions fail
              -- get >>= \preState -> do
              --             let badBets = concatMap toEdgeCases availableActs
              --             forM_ badBets $ \badBet -> do
              --               let res :: Either (GameErrorBundle BetSize) () = evalStateT (emulateAction (MkPlayerAction . flip (PlayerAction pos) Hero $ badBet)) preState
              --               case res of
              --                 Left e -> pure ()
              --                 Right () -> liftIO $ assertBool
              --                       (printf "handNum: %d\norigAct: %s\nbadBet: %s\nhand: %s"
              --                        ix
              --                        (show act)
              --                        (show badBet)
              --                        (_handText hand))
              --                       False
              case act of
                        MkPlayerAction (PlayerAction _pos playerAct _isHero) -> do
                            let actValid = any (playerAct `betMatches`) availableActs
                            liftIO $ assertBool
                                    (printf "handNum: %d\nq: %s pos: %s\nact:%s\navailables: %s\nhand:%s"
                                      ix
                                      (show q)
                                      (show pos)
                                      (show act)
                                      (show availableActs)
                                      (_handText hand))
                                    actValid
                        _ -> pure ()
              emulateAction act
              pure ()
    toEdgeCases :: BetAction (IxRange BetSize) -> [BetAction BetSize]
    toEdgeCases (Call amount) = [Call 0, Call 10000]
    toEdgeCases (Raise _ amount)  = [Raise 0 0, Raise 0 10000]
    toEdgeCases (AllInRaise _ amount)  = [AllInRaise 0 0, AllInRaise 0 10000]
    toEdgeCases (Bet amount)  = [Bet 0, Bet 10000]
    toEdgeCases (AllIn amount)  = [AllIn 0, AllIn 10000]
    toEdgeCases Fold = []
    toEdgeCases FoldTimeout = []
    toEdgeCases Check = []
    toEdgeCases CheckTimeOut = []
    toEdgeCases OtherAction = []

    betMatches :: BetAction BetSize -> BetAction (IxRange BetSize) -> Bool
    betMatches (Call amount)(Call r) = within amount r
    betMatches (Raise _ amount) (Raise _ r) = within amount r
    betMatches (AllInRaise _ amount) (AllInRaise _ r) = amount `within` r
    betMatches (Bet amount) (Bet r) = amount `within` r
    betMatches (AllIn amount) (AllIn r) = amount `within` r
    betMatches Fold Fold = True
    betMatches FoldTimeout FoldTimeout = True
    betMatches Check Check = True
    betMatches CheckTimeOut Check = True
    betMatches OtherAction _ = True
    betMatches _ _ = False