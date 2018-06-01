{-# LANGUAGE OverloadedStrings #-}

module Amazon.Scraper.Offers where

import qualified Data.ByteString.Lazy        as BSL
import           Data.ByteString.Lazy.Char8  (pack, unpack, dropWhile)
import qualified Data.ByteString.Lazy.Char8  as BSLC
import           Extra.Util.Func             (cleanupText, extractCurrency)
import           Data.Maybe                  (fromMaybe)
import           Text.HTML.Scalpel.Core
import           Text.Read

type Webpage = BSL.ByteString

-- | Contains all scraped data from the Amazon Offers Webpage.
data AmazonProduct = AmazonProduct { title :: Maybe BSL.ByteString,
                                     image :: Maybe BSL.ByteString,
                                     azASIN :: Maybe BSL.ByteString,
                                     offers :: [AmazonOffer]
                                   } deriving (Show)

-- | Contains all scraped data of a single offer from the Amazon Offers Webpage.
data AmazonOffer = AmazonOffer { price :: Maybe Float,
                                 shipping :: Maybe Float,
                                 totalPrice :: Maybe Float,
                                 condition :: Maybe BSL.ByteString,
                                 description :: Maybe BSL.ByteString,
                                 sellerName :: Maybe BSL.ByteString,
                                 sellerRating :: Maybe BSL.ByteString
                               } deriving (Show)


-- | Creates an 'AmazonProduct' based off Amazon Offers webpage html.
createAmazonProduct :: Webpage -> AmazonProduct
createAmazonProduct webpage = AmazonProduct { title = scrapeTitle webpage,
                                              image = scrapeImage webpage,
                                              azASIN = scrapeASIN webpage,
                                              offers = scrapeOffers webpage
                                              }
-- | Scrapes product title from webpage.
scrapeTitle :: Webpage -> Maybe (BSL.ByteString)
scrapeTitle webpage = fmap cleanupText $ scrapeStringLike webpage (text titleSelector)
  where titleSelector = "h1" @: ["role" @= "main"]

-- | Scrapes image from webpage.
scrapeImage :: Webpage -> Maybe (BSL.ByteString)
scrapeImage webpage = scrapeStringLike webpage (attr "src" imageSelector)
  where imageSelector = "img" @: ["alt" @= "Return to product information"]

-- | Scrapes Amazon ASIN from webpage.
scrapeASIN :: Webpage -> Maybe (BSL.ByteString)
scrapeASIN webpage = fmap getASIN $ scrapeStringLike webpage (html asinSelector)
  where asinSelector = "div" @: ["id" @= "olpProductImage"]
        getASIN = BSLC.takeWhile (/= '/') . BSLC.dropWhile (/= 'B')


-- | Creates an 'AmazonOffer' based off of the offer html.
createAmazonOffer :: BSL.ByteString -> AmazonOffer
createAmazonOffer offHtml = AmazonOffer { price = offPrice,
                                          shipping = offShipping,
                                          totalPrice = (+) <$> offPrice <*> offShipping,
                                          condition = scrapeOfferCondition offHtml,
                                          description = scrapeOfferDescription offHtml,
                                          sellerName = scrapeOfferSellerName offHtml,
                                          sellerRating = scrapeOfferSellerRating offHtml}
  where offPrice = scrapeOfferPrice offHtml
        offShipping = scrapeOfferShipping offHtml


-- | Scrapes all 'AmazonOffer's from webpage.
scrapeOffers :: Webpage -> [AmazonOffer]
scrapeOffers webpage = fmap createAmazonOffer $ fromMaybe [] $ scrapeStringLike webpage (htmls offersSelector)
  where offersSelector = "div" @: ["class" @= "a-row a-spacing-mini olpOffer"]

-- | Scrape offer price from offer html.
scrapeOfferPrice :: BSL.ByteString -> Maybe Float
scrapeOfferPrice offHtml = extractCurrency $ fromMaybe "" $ scrapeStringLike offHtml (text priceSelector)
  where priceSelector = "span" @: ["class" @= "a-size-large a-color-price olpOfferPrice a-text-bold"]

-- | Scrape offer shipping from offer html.
scrapeOfferShipping :: BSL.ByteString -> Maybe Float
scrapeOfferShipping offHtml = extractShipping $ fromMaybe " " $ scrapeStringLike offHtml (text shippingSelector)
  where shippingSelector = "p" @: ["class" @= "olpShippingInfo"]
        extractShipping s = if (BSLC.elem 'F' s) then Just 0.00 else extractCurrency s

-- | Scrape offer condition from offer html.
scrapeOfferCondition :: BSL.ByteString -> Maybe BSL.ByteString
scrapeOfferCondition offHtml = fmap cleanUsedConditionText $ scrapeStringLike offHtml (text conditionSelector)
  where conditionSelector = "span" @: ["class" @= "a-size-medium olpCondition a-text-bold"]
        cleanUsedConditionText = cleanupText . BSLC.drop 2 . BSLC.dropWhile (/= '-')

-- | Scrape offer description from offer html.
scrapeOfferDescription :: BSL.ByteString -> Maybe BSL.ByteString
scrapeOfferDescription offHtml = fmap cleanupText $ scrapeStringLike offHtml (text descriptionSelector)
  where descriptionSelector = "div" @: ["class" @= "expandedNote"]

-- | Scrape offer seller name from offer html.
scrapeOfferSellerName :: BSL.ByteString -> Maybe BSL.ByteString
scrapeOfferSellerName offHtml = fmap cleanupText $ scrapeStringLike offHtml (text sellerNameSelector)
  where sellerNameSelector = "h3" @: ["class" @= "a-spacing-none olpSellerName"]

-- | Scrape offer seller rating from offer html.
scrapeOfferSellerRating :: BSL.ByteString -> Maybe BSL.ByteString
scrapeOfferSellerRating offHtml = fmap cleanupRating $ scrapeStringLike offHtml (text sellerRatingSelector)
  where sellerRatingSelector = "p" @: ["class" @= "a-spacing-small"]
        cleanupRating = cleanupText . BSLC.drop 45
