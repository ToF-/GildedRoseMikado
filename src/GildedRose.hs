module GildedRose where

type GildedRose = [Item]

type Name    = String
type SellIn  = Int
type Quality = Int

data Item = Item Name SellIn Quality
  deriving (Eq)

instance Show Item where
  show (Item name sellIn quality) =
    name ++ ", " ++ show sellIn ++ ", " ++ show quality

updateQuality :: GildedRose -> GildedRose
updateQuality = map updateQualityItem
  where
    updateQualityItem (Item name sellIn quality) =
      let
        quality' =
          if name /= "Aged Brie"
             && name /= "Backstage passes to a TAFKAL80ETC concert"
          then
            if quality > 0
            then
              if name /= "Sulfuras, Hand of Ragnaros"
              then quality - 1
              else quality
            else quality
          else
            if quality < 50
            then
              quality + 1 +
                (if name == "Backstage passes to a TAFKAL80ETC concert"
                 then
                   if sellIn < 11
                   then
                     if quality < 49
                     then
                       1 + (if sellIn < 6
                            then
                              if quality < 48
                              then 1
                              else 0
                            else 0)
                     else 0
                   else 0
                 else 0)
            else quality

        sellIn' =
          if name /= "Sulfuras, Hand of Ragnaros"
          then sellIn - 1
          else sellIn
      in
        if sellIn' < 0
        then
          if name /= "Aged Brie"
          then
            if name /= "Backstage passes to a TAFKAL80ETC concert"
            then
              if quality' > 0
              then
                if name /= "Sulfuras, Hand of Ragnaros"
                then (Item name sellIn' (quality' - 1))
                else (Item name sellIn' quality')
              else (Item name sellIn' quality')
            else (Item name sellIn' (quality' - quality'))
          else increaseQuality (Item name sellIn' quality')
        else (Item name sellIn' quality')

increaseQuality :: Item -> Item
increaseQuality (Item name sellIn quality) | quality < 50 = Item name sellIn (succ quality)
increaseQuality item = item
