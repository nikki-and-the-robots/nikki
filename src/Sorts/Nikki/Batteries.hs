
module Sorts.Nikki.Batteries where


import Data.Map (Map, (!), empty, insert)
import Data.Set (Set, fromList, member, )
import Data.List (nubBy)
import Data.Function
import Data.Indexable (Index, Indexable(..), fmapWithIndex)

import Control.Monad.State.Strict (StateT)

import Physics.Chipmunk

import Utils

import Base

import Sorts.Battery (Battery(..), unwrapBattery)
import Sorts.Nikki.Types (NSort(..))


mkBatteryMap :: Indexable Object_ -> Map Shape (Index, Chipmunk)
mkBatteryMap mainLayer =
    foldl folding empty $ ftoList $ values mainLayer
  where
    folding :: Map Shape (Index, Chipmunk) -> (Index, Object_) -> Map Shape (Index, Chipmunk)
    folding akk (index, object) = case unwrapBattery object of
        Nothing -> akk
        Just (_, battery) -> insertBattery akk index battery

    insertBattery :: Map Shape (Index, Chipmunk) -> Index -> Battery -> Map Shape (Index, Chipmunk)
    insertBattery akk index battery =
        foldl (\ innerAkk shape -> insert shape (index, chipmunk battery) innerAkk)
        akk
        (shapes $ chipmunk battery)


-- | gets called from Nikki's updating function
removeBatteries :: NSort -> Configuration -> Contacts -> Map Shape (Index, Chipmunk)
    -> StateT (Scene Object_ -> Scene Object_) IO ()
removeBatteries sort config contacts batteryMap = do
    let toBeRemovedChipmunks = nubBy ((==) `on` fst) $ map (batteryMap !) $ ftoList (batteries contacts)
    -- play a single sound in case of battery collision
    when (not $ null toBeRemovedChipmunks) $ do
        io $ triggerSound config $ batteryCollectSound sort
        -- consume every battery
        let indexSet = fromList $ map fst toBeRemovedChipmunks
        pushSceneChange (objects .> gameMainLayer ^: markAsConsumed indexSet)
        pushSceneChange (addPower (fromIntegral (length toBeRemovedChipmunks)))
        forM_ toBeRemovedChipmunks $ \ (_, battery) -> do
            io $ removeChipmunk battery
  where
    addPower :: Integer -> Scene Object_ -> Scene Object_
    addPower n = batteryPower .> firstAStrict ^: (+ n)

    markAsConsumed :: Set Index -> Indexable Object_ -> Indexable Object_
    markAsConsumed toBeRemovedIndices =
        fmapWithIndex $ \ i object ->
            if i `member` toBeRemovedIndices then
                case unwrapBattery object of
                    Just (sort, battery) ->
                        Object_ sort battery{consumed = True}
                    Nothing -> object
              else
                object
